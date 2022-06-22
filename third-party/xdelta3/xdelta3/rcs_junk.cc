typedef struct _RcsWalker               RcsWalker;
typedef struct _RcsFile                 RcsFile;
typedef struct _RcsVersion              RcsVersion;
typedef struct _RcsStats                RcsStats;
typedef struct _IntStat                 IntStat;
typedef struct _DblStat                 DblStat;
typedef struct _BinCounter              BinCounter;
typedef struct _ConfigOption            ConfigOption;

struct _RcsWalker {
  void*    (* initialize)    (void);
  int      (* finalize)      (RcsStats* stats, void* data);
  int      (* onefile)       (RcsFile* rcs, RcsStats* stats, void* data);
  int      (* dateorder)     (RcsFile* rcs, RcsVersion* v, void* data);
  int      (* delta_orig)    (RcsFile* rcs, RcsVersion* from, RcsVersion *to, void* data);
  int      (* delta_date)    (RcsFile* rcs, RcsVersion* from, RcsVersion *to, void* data);
  int      min_versions;
  int      max_versions;
  gboolean write_files;
};

struct _RcsVersion {
  RcsFile    *rcs;
  time_t      date;
  int         dateseq;
  int         chain_length;
  char       *vname;
  off_t       size;
  int         cc;
  guint8*     segment;
  char       *filename;
  RcsVersion *parent;
  GSList     *children;
  guint       on_trunk : 1;
};

struct _RcsFile {
  char       *filename;
  char       *copyname;
  char       *headname;

  int         version_count;
  int         forward_count;
  int         reverse_count;
  int         branch_count;

  RcsVersion *versions;
  RcsVersion **versions_date;

  RcsVersion *head_version;
  RcsVersion *root_version;

  off_t       total_size;

  guint       atflag : 1;
};

struct _RcsStats {
  BinCounter *avg_version_size;
  IntStat* version_stat;
  IntStat* forward_stat;
  IntStat* reverse_stat;
  IntStat* branch_stat;
  IntStat* unencoded_stat;
  IntStat* literal_stat;
};

struct _IntStat {
  const char* name;
  int count;
  long long sum;
  long long min;
  long long max;

  GArray *values;
};

struct _DblStat {
  const char* name;
  int count;
  double sum;
  double min;
  double max;

  GArray *values;
};

struct _BinCounter {
  const char *name;
  GPtrArray  *bins;
};

enum _ConfigArgument {
  CO_Required,
  CO_Optional,
  CO_None
};

typedef enum _ConfigArgument ConfigArgument;

enum _ConfigOptionType {
  CD_Bool,
  CD_Int32,
  CD_Double,
  CD_String
};

typedef enum _ConfigOptionType ConfigOptionType;

enum _ConfigStyle {
  CS_Ignore,
  CS_UseAsFile,
  CS_Use
};

typedef enum _ConfigStyle ConfigStyle;

struct _ConfigOption {
  const char       *name;
  const char       *abbrev;
  ConfigStyle       style;
  ConfigArgument    arg;
  ConfigOptionType  type;
  void             *value;
  gboolean          found;
};

/* RCS inspection stuff
 */

void                rcswalk_init   (void);
int            rcswalk        (RcsWalker *walker, const char* copy_base);
void                rcswalk_report (RcsStats* stats);

IntStat*            stat_int_new      (const char* name);
void                stat_int_add_item (IntStat* stat, long long v);
void                stat_int_report   (IntStat* stat);

DblStat*            stat_dbl_new      (const char* name);
void                stat_dbl_add_item (DblStat* stat, double v);
void                stat_dbl_report   (DblStat* stat);

BinCounter*         stat_bincount_new      (const char* name);
void                stat_bincount_add_item (BinCounter* bc, int bin, double val);
void                stat_bincount_report   (BinCounter* bc);

/* Experiment configuration stuff
 */

void                config_register   (ConfigOption *opts, int nopts);
int            config_parse      (const char* config_file);
int            config_done       (void);
void                config_help       (void);
void                config_set_string (const char* var, const char* val);
int            config_clear_dir  (const char* dir);
int            config_create_dir (const char* dir);
FILE*               config_output     (const char* fmt, ...);

#ifdef __cplusplus
}
#endif

#endif
#include "rcswalk.h"
#include "edsio.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>
#include <dirent.h>
#include <unistd.h>
#include <math.h>

#undef BUFSIZE
#define BUFSIZE (1<<14)

char       *tmp_file_1;
gboolean    tmp_file_1_free = TRUE;
char       *tmp_file_2;
gboolean    tmp_file_2_free = TRUE;

int         skip_count;
int         small_count;
int         large_count;
int         process_count;

extern time_t str2time (char const *, time_t, long);

static guint8 readbuf[BUFSIZE];

static const char* rcswalk_input_dir = NULL;
static const char* config_output_base = NULL;
static const char* config_output_dir = NULL;
static const char* rcswalk_experiment = NULL;

static ConfigOption rcswalk_options[] = {
  { "rcswalk_experiment", "ex", CS_Use,       CO_Required, CD_String, & rcswalk_experiment },
  { "rcs_input_dir",      "id", CS_UseAsFile, CO_Required, CD_String, & rcswalk_input_dir }
};

static ConfigOption config_options[] = {
  { "config_output_base", "ob", CS_Ignore, CO_Required, CD_String, & config_output_base }
};


void
rcswalk_free_segment (RcsVersion *v)
{
  if (v->segment)
    g_free (v->segment);

  if (v->filename == tmp_file_1)
    tmp_file_1_free = TRUE;
  else if (v->filename == tmp_file_2)
    tmp_file_2_free = TRUE;
  else if (v->filename)
    g_free (v->filename);

  v->segment = NULL;
  v->filename = NULL;
}

int
rcswalk_checkout (RcsFile* rcs, RcsWalker* walker, RcsVersion *v)
{
  FILE* out;
  char cmdbuf[1024];
  int nread;
  int alloc = BUFSIZE;
  int pos = 0;

  sprintf (cmdbuf, "co -ko -p%s %s 2>/dev/null\n", v->vname, rcs->filename);

  g_assert (! v->segment);

  v->segment = g_malloc (alloc);

  if (! (out = popen (cmdbuf, "r")))
    {
      g_warning ("popen failed: %s: %s", cmdbuf, g_strerror (errno));
      return errno;
    }

  for (;;)
    {
      nread = fread (readbuf, 1, BUFSIZE, out);

      if (nread == 0)
	break;

      if (nread < 0)
	{
	  g_warning ("fread failed: %s", g_strerror (errno));
	  return errno;
	}

      if (pos + nread > alloc)
	{
	  alloc *= 2;
	  v->segment = g_realloc (v->segment, alloc);
	}

      memcpy (v->segment + pos, readbuf, nread);

      pos += nread;
    }

  if (pclose (out) < 0)
    {
      g_warning ("pclose failed");
      return errno;
    }

  v->size = pos;

  if (walker->write_files)
    {
      char* file = NULL;

      if (! file && tmp_file_1_free)
	{
	  file = tmp_file_1;
	  tmp_file_1_free = FALSE;
	}

      if (! file && tmp_file_2_free)
	{
	  file = tmp_file_2;
	  tmp_file_2_free = FALSE;
	}

      g_assert (file);

      v->filename = file;

      if (! (out = fopen (file, "w")))
	{
	  g_warning ("fopen failed: %s\n", file);
	  return errno;
	}

      if (fwrite (v->segment, v->size, 1, out) != 1)
	{
	  g_warning ("fwrite failed: %s\n", file);
	  return errno;
	}

      if (fclose (out) < 0)
	{
	  g_warning ("fclose failed: %s\n", file);
	  return errno;
	}
    }

  return 0;
}

int
rcswalk_delta_date (RcsFile* rcs, RcsWalker* walker, void* data)
{
  int i;
  int ret;
  RcsVersion *vf = NULL;
  RcsVersion *vt = NULL;

  for (i = 0; i < (rcs->version_count-1); i += 1)
    {
      vf = rcs->versions_date[i+1];
      vt = rcs->versions_date[i];

      if (! vt->segment && (ret = rcswalk_checkout (rcs, walker, vt))) {
	return ret;
      }

      if ((ret = rcswalk_checkout (rcs, walker, vf))) {
	return ret;
      }

      if ((ret = walker->delta_date (rcs, vf, vt, data))) {
	return ret;
      }

      rcswalk_free_segment (vt);
    }

  if (vf) rcswalk_free_segment (vf);
  if (vt) rcswalk_free_segment (vt);

  return 0;
}

int
rcswalk_delta_orig (RcsFile* rcs, RcsWalker* walker, RcsVersion* version, int *count, void* data)
{
  int ret;
  GSList *c;
  RcsVersion *child;

  for (c = version->children; c; c = c->next)
    {
      gboolean reverse;

      child = c->data;

      if (! version->segment)
	{
	  if ((ret = rcswalk_checkout (rcs, walker, version))) {
	    return ret;
	  }
	}

      if ((ret = rcswalk_checkout (rcs, walker, child))) {
	return ret;
      }

      reverse = version->on_trunk && child->on_trunk;

      (* count) += 1;

      if ((ret = walker->delta_orig (rcs, reverse ? child : version, reverse ? version : child, data))) {
	return ret;
      }

      rcswalk_free_segment (version);

      if ((ret = rcswalk_delta_orig (rcs, walker, child, count, data))) {
	return ret;
      }
    }

  rcswalk_free_segment (version);
  return 0;
}

int
rcswalk_dateorder (RcsFile* rcs, RcsWalker *walker, RcsStats *stats, void* data)
{
  int i, ret;

  for (i = 0; i < rcs->version_count; i += 1)
    {
      RcsVersion *v = rcs->versions_date[i];

      if ((ret = rcswalk_checkout (rcs, walker, v))) {
	return ret;
      }

      stat_bincount_add_item (stats->avg_version_size, i, v->size);

      if ((ret = walker->dateorder (rcs, v, data))) {
	return ret;
      }

      rcswalk_free_segment (v);
    }

  return 0;
}

gboolean
rcswalk_match (char** line_p, char* str)
{
  int len = strlen (str);

  if (strncmp (*line_p, str, len) == 0)
    {
      (*line_p) += len;
      return TRUE;
    }

  return FALSE;
}

void
rcswalk_find_parent (RcsFile *rcs, GHashTable* hash, RcsVersion *v)
{
  char *lastdot;
  char  mbuf[1024];
  int   lastn;
  RcsVersion *p;

  strcpy (mbuf, v->vname);

  if (! (lastdot = strchr (mbuf, '.')))
    abort ();

  if (! (lastdot = strchr (lastdot+1, '.')))
    v->on_trunk = TRUE;

  lastdot = strrchr (mbuf, '.');
  lastn = atoi (lastdot + 1);

  do
    {
      if (lastn == 1)
	{
	  (*lastdot) = 0;

	  if (strcmp (mbuf, "1") == 0)
	    {
	      /* Assuming the first version is always "1.1".
	       */
	      rcs->root_version = v;
	      return;
	    }
	  else if (! (lastdot = strrchr (mbuf, '.')))
	    {
	      int i = 1;
	      int br = atoi (mbuf) - 1;
	      RcsVersion *p2 = NULL;

	      /* Now we have something like "2.1" and need to
	       * search for the highest "1.x" version.
	       */

	      do
		{
		  sprintf (mbuf, "%d.%d", br, i++);
		  p = p2;
		}
	      while ((p2 = g_hash_table_lookup (hash, mbuf)));

	      if (p == NULL)
		{
		  rcs->root_version = v;
		  return;
		}

	      break;
	    }
	  else
	    {
	      /* 1.2.3.1 => 1.2 */
	      (*lastdot) = 0;
	      lastdot = strrchr (mbuf, '.');
	      lastn = atoi (lastdot + 1);
	    }
	}
      else
	{
	  lastn -= 1;
	  sprintf (lastdot, ".%d", lastn);
	}
    }
  while (! (p = g_hash_table_lookup (hash, mbuf)));

  g_assert (p);

  v->parent = p;

  p->children = g_slist_prepend (p->children, v);
}

int
rcswalk_traverse_graph (RcsFile* rcs, RcsVersion* version, RcsVersion *parent)
{
  GSList *c;
  int distance = -1;

  version->cc = g_slist_length (version->children);

  if (version->cc > 1)
    rcs->branch_count += (version->cc - 1);

  if (parent)
    {
      /* Insure that there is proper date ordering. */
      if (version->date <= parent->date)
	version->date = parent->date + 1;

      if (parent->on_trunk && version->on_trunk)
	rcs->reverse_count += 1;
      else
	rcs->forward_count += 1;
    }

  for (c = version->children; c; c = c->next)
    {
      int c_dist = rcswalk_traverse_graph (rcs, c->data, version);

      distance = MAX (distance, c_dist);
    }

  if (version == rcs->head_version)
    distance = 0;

  if (distance >= 0)
    {
      version->chain_length = distance;

      return distance + 1;
    }

  return -1;
}

void
rcswalk_compute_chain_length (RcsFile* rcs, RcsVersion* version, RcsVersion *parent)
{
  GSList *c;

  if (! parent)
    {
      g_assert (version->chain_length >= 0);
    }
  else if (version->chain_length < 0)
    {
      version->chain_length = parent->chain_length + 1;
    }

  for (c = version->children; c; c = c->next)
    {
      rcswalk_compute_chain_length (rcs, c->data, version);
    }
}

int
rcswalk_date_compare (const void* a, const void* b)
{
  RcsVersion **ra = (void*) a;
  RcsVersion **rb = (void*) b;

  return (*ra)->date - (*rb)->date;
}

int
rcswalk_build_graph (RcsFile* rcs)
{
  GHashTable* hash = g_hash_table_new (g_str_hash, g_str_equal);
  int i;

  for (i = 0; i < rcs->version_count; i += 1)
    g_hash_table_insert (hash, rcs->versions[i].vname, rcs->versions + i);

  for (i = 0; i < rcs->version_count; i += 1)
    {
      RcsVersion *v = rcs->versions + i;

      v->chain_length = -1;
      v->rcs = rcs;

      rcswalk_find_parent (rcs, hash, v);
    }

  rcs->head_version = g_hash_table_lookup (hash, rcs->headname);

  rcswalk_traverse_graph (rcs, rcs->root_version, NULL);

  rcswalk_compute_chain_length (rcs, rcs->root_version, NULL);

  for (i = 0; i < rcs->version_count; i += 1)
    rcs->versions_date[i] = rcs->versions + i;

  qsort (rcs->versions_date, rcs->version_count, sizeof (RcsVersion*), & rcswalk_date_compare);

  for (i = 0; i < rcs->version_count; i += 1)
    {
      RcsVersion *v = rcs->versions_date[i];

      v->dateseq = i;
    }

  g_hash_table_destroy (hash);

  return 0;
}

#define HEAD_STATE 0
#define BAR_STATE 1
#define REV_STATE 2
#define DATE_STATE 3

int
rcswalk_load (RcsFile *rcs, gboolean *skip)
{
  FILE* rlog;
  char cmdbuf[1024];
  char oneline[1024], *oneline_p;
  char rbuf[1024];
  int version_i = 0, ret;
  int read_state = HEAD_STATE;

  sprintf (cmdbuf, "rlog %s", rcs->filename);

  if (! (rlog = popen (cmdbuf, "r")))
    {
      g_warning ("popen failed: %s", cmdbuf);
      return errno;
    }

  rcs->headname = NULL;

  while (fgets (oneline, 1024, rlog))
    {
      oneline_p = oneline;

      if (read_state == HEAD_STATE && rcswalk_match (& oneline_p, "total revisions: "))
	{
	  if (sscanf (oneline_p, "%d", & rcs->version_count) != 1)
	    goto badscan;

	  rcs->versions = g_new0 (RcsVersion, rcs->version_count);
	  rcs->versions_date = g_new (RcsVersion*, rcs->version_count);
	  read_state = BAR_STATE;
	}
      else if (read_state == HEAD_STATE && rcswalk_match (& oneline_p, "head: "))
	{
	  if (sscanf (oneline_p, "%s", rbuf) != 1)
	    goto badscan;

	  rcs->headname = g_strdup (rbuf);
	  read_state = HEAD_STATE; /* no change */
	}
      else if (read_state == BAR_STATE && rcswalk_match (& oneline_p, "----------------------------"))
	{
	  read_state = REV_STATE;
	}
      else if (read_state == REV_STATE && rcswalk_match (& oneline_p, "revision "))
	{
	  if (version_i >= rcs->version_count)
	    {
	      /* jkh likes to insert the rlog of one RCS file into the log
	       * message of another, and this can confuse things.  Why, oh why,
	       * doesn't rlog have an option to not print the log?
	       */
	      fprintf (stderr, "rcswalk: too many versions: skipping file %s\n", rcs->filename);
	      *skip = TRUE;
	      skip_count += 1;
	      pclose (rlog);
	      return 0;
	    }

	  if (sscanf (oneline_p, "%s", rbuf) != 1)
	    goto badscan;

	  rcs->versions[version_i].vname = g_strdup (rbuf);
	  read_state = DATE_STATE;

	  g_assert (rcs->versions[version_i].vname);
	}
      else if (read_state == DATE_STATE && rcswalk_match (& oneline_p, "date: "))
	{
	  char* semi = strchr (oneline_p, ';');

	  if (! semi)
	    goto badscan;

	  strncpy (rbuf, oneline_p, semi - oneline_p);

	  rbuf[semi - oneline_p] = 0;

	  rcs->versions[version_i].date = str2time (rbuf, 0, 0);

	  version_i += 1;
	  read_state = BAR_STATE;
	}
    }

  if (! rcs->headname)
    {
      fprintf (stderr, "rcswalk: no head version: skipping file %s\n", rcs->filename);
      *skip = TRUE;
      skip_count += 1;
      pclose (rlog);
      return 0;
    }

  if (pclose (rlog) < 0)
    {
      g_warning ("pclose failed: %s", cmdbuf);
      return errno;
    }

  if ((ret = rcswalk_build_graph (rcs))) {
    return ret;
  }

  return 0;

 badscan:

  pclose (rlog);

  g_warning ("rlog syntax error");
  return -1;
}

void
rcswalk_free (RcsFile* rcs)
{
  int i;

  for (i = 0; i < rcs->version_count; i += 1)
    {
      g_free (rcs->versions[i].vname);
      g_slist_free (rcs->versions[i].children);
    }

  g_free (rcs->filename);
  g_free (rcs->headname);
  g_free (rcs->versions);
  g_free (rcs->versions_date);
  g_free (rcs);
}

int
rcswalk_one (char* rcsfile, char* copyfile, RcsWalker* walker, RcsStats* stats, void* data)
{
  RcsFile* rcs;
  int i, ret;
  long long maxsize = 0;
  gboolean skip = FALSE;

  rcs = g_new0 (RcsFile, 1);

  rcs->filename = g_strdup (rcsfile);
  rcs->copyname = copyfile;

  if ((ret = rcswalk_load (rcs, & skip))) {
    return ret;
  }

  if (walker->min_versions > rcs->version_count)
    {
      small_count += 1;
      skip = TRUE;
    }

  if (walker->max_versions < rcs->version_count)
    {
      large_count += 1;
      skip = TRUE;
    }

  if (! skip)
    {
      process_count += 1;

      if (walker->dateorder && (ret = rcswalk_dateorder (rcs, walker, stats, data))) {
	return ret;
      }

      if (walker->delta_orig)
	{
	  int count = 0;

	  if ((ret = rcswalk_delta_orig (rcs, walker, rcs->root_version, & count, data))) {
	    return ret;
	  }

	  g_assert (count == (rcs->version_count - 1));
	}

      if (walker->delta_date && (ret = rcswalk_delta_date (rcs, walker, data))) {
	return ret;
      }

      for (i = 0; i < rcs->version_count; i += 1)
	{
	  rcs->total_size += rcs->versions[i].size;
	  maxsize = MAX (rcs->versions[i].size, maxsize);
	}

      stat_int_add_item (stats->version_stat, rcs->version_count);
      stat_int_add_item (stats->forward_stat, rcs->forward_count);
      stat_int_add_item (stats->reverse_stat, rcs->reverse_count);
      stat_int_add_item (stats->branch_stat, rcs->branch_count);
      stat_int_add_item (stats->unencoded_stat, rcs->total_size);
      stat_int_add_item (stats->literal_stat, maxsize);

      if (walker->onefile && (ret = walker->onefile (rcs, stats, data))) {
	return ret;
      }
    }

  rcswalk_free (rcs);

  return 0;
}

int
rcswalk_dir (const char* dir, RcsWalker* walker, RcsStats* stats, void* data, const char* copy_dir)
{
  int ret;
  DIR* thisdir;
  struct dirent* ent;

  if (copy_dir && (ret = config_create_dir (copy_dir))) {
    return ret;
  }

  if (! (thisdir = opendir (dir)))
    {
      g_warning ("opendir failed: %s", dir);
      return errno;
    }

  while ((ent = readdir (thisdir)))
    {
      char* name = ent->d_name;
      int len;
      struct stat buf;
      char* fullname;
      char* copyname = NULL;

      if (strcmp (name, ".") == 0)
	continue;

      if (strcmp (name, "..") == 0)
	continue;

      len = strlen (name);

      fullname = g_strdup_printf ("%s/%s", dir, name);

      if (copy_dir)
	copyname = g_strdup_printf ("%s/%s", copy_dir, name);

      if (len > 2 && strcmp (name + len - 2, ",v") == 0)
	{
	  if ((ret = rcswalk_one (fullname, copyname, walker, stats, data))) {
	    goto abort;
	  }
	}
      else
	{
	  if (stat (fullname, & buf) < 0)
	    {
	      g_warning ("stat failed: %s\n", fullname);
	      goto abort;
	    }

	  if (S_ISDIR (buf.st_mode))
	    {
	      if ((ret = rcswalk_dir (fullname, walker, stats, data, copyname))) {
		goto abort;
	      }
	    }
	}

      g_free (fullname);

      if (copyname)
	g_free (copyname);
    }

  if (closedir (thisdir) < 0)
    {
      g_warning ("closedir failed: %s", dir);
      return errno;
    }

  return 0;

 abort:

  if (thisdir)
    closedir (thisdir);

  return -1;
}

void
rcswalk_init (void)
{
  config_register (rcswalk_options, ARRAY_SIZE (rcswalk_options));
}

int
rcswalk (RcsWalker *walker, const char* copy_base)
{
  void* data = NULL;
  RcsStats stats;
  int ret;

  skip_count = 0;
  small_count = 0;
  process_count = 0;
  large_count = 0;

  memset (& stats, 0, sizeof (stats));

  stats.avg_version_size = stat_bincount_new ("AvgVersionSize"); /* @@@ leak */
  stats.version_stat = stat_int_new ("Version"); /* @@@ leak */
  stats.forward_stat = stat_int_new ("Forward"); /* @@@ leak */
  stats.reverse_stat = stat_int_new ("Reverse"); /* @@@ leak */
  stats.branch_stat  = stat_int_new ("Branch"); /* @@@ leak */
  stats.unencoded_stat = stat_int_new ("Unencoded"); /* @@@ leak */
  stats.literal_stat   = stat_int_new ("Literal"); /* @@@ leak */

  tmp_file_1 = g_strdup_printf ("%s/rcs1.%d", g_get_tmp_dir (), (int) getpid ());
  tmp_file_2 = g_strdup_printf ("%s/rcs2.%d", g_get_tmp_dir (), (int) getpid ());

  if (walker->initialize)
    data = walker->initialize ();

  if ((ret = rcswalk_dir (rcswalk_input_dir, walker, & stats, data, copy_base))) {
    return ret;
  }

  if (walker->finalize)
    {
      if ((ret = walker->finalize (& stats, data))) {
	return ret;
      }
    }

  unlink (tmp_file_1);
  unlink (tmp_file_2);

  fprintf (stderr, "rcswalk: processed %d files: too small %d; too large: %d; damaged: %d\n", process_count, small_count, large_count, skip_count);

  return 0;
}

/* Statistics
 */

void
rcswalk_report (RcsStats* set)
{
  stat_bincount_report (set->avg_version_size);
  stat_int_report (set->version_stat);
  stat_int_report (set->forward_stat);
  stat_int_report (set->reverse_stat);
  stat_int_report (set->branch_stat);
  stat_int_report (set->unencoded_stat);
  stat_int_report (set->literal_stat);
}

/* Int stat
 */
IntStat*
stat_int_new (const char* name)
{
  IntStat* s = g_new0 (IntStat, 1);

  s->name = name;
  s->values = g_array_new (FALSE, FALSE, sizeof (long long));

  return s;
}

void
stat_int_add_item (IntStat* stat, long long v)
{
  if (! stat->count)
    stat->min = v;
  stat->count += 1;
  stat->min = MIN (v, stat->min);
  stat->max = MAX (v, stat->max);
  stat->sum += v;

  g_array_append_val (stat->values, v);
}

double
stat_int_stddev (IntStat *stat)
{
  double f = 0;
  double m = (double) stat->sum / (double) stat->count;
  double v;
  int i;

  for (i = 0; i < stat->count; i += 1)
    {
      long long x = g_array_index (stat->values, long long, i);

      f += (m - (double) x) * (m - (double) x);
    }

  v = f / (double) stat->count;

  return sqrt (v);
}

int
ll_comp (const void* a, const void* b)
{
  const long long* lla = a;
  const long long* llb = b;
  return (*lla) - (*llb);
}

void
stat_int_histogram (IntStat *stat)
{
  int i, consec;
  long long cum = 0;

  FILE* p_out;
  FILE* s_out;

  if (! (p_out = config_output ("%s.pop.hist", stat->name)))
    abort ();

  if (! (s_out = config_output ("%s.sum.hist", stat->name)))
    abort ();

  qsort (stat->values->data, stat->count, sizeof (long long), ll_comp);

  for (i = 0; i < stat->count; i += consec)
    {
      long long ix = g_array_index (stat->values, long long, i);

      for (consec = 1; (i+consec) < stat->count; consec += 1)
	{
	  long long jx = g_array_index (stat->values, long long, i+consec);

	  if (ix != jx)
	    break;
	}

      cum += consec * g_array_index (stat->values, long long, i);

      fprintf (p_out, "%qd, %0.3f\n", g_array_index (stat->values, long long, i), (double) (i+consec) / (double) stat->count);
      fprintf (s_out, "%qd, %0.3f\n", g_array_index (stat->values, long long, i), (double) cum / (double) stat->sum);
    }

  if (fclose (p_out) < 0 || fclose (s_out) < 0)
    {
      g_error ("fclose failed\n");
    }
}

void
stat_int_report (IntStat* stat)
{
  FILE* out;

  if (! (out = config_output ("%s.stat", stat->name)))
    abort ();

  fprintf (out, "Name: %s\n", stat->name);
  fprintf (out, "Count: %d\n", stat->count);
  fprintf (out, "Min: %qd\n", stat->min);
  fprintf (out, "Max: %qd\n", stat->max);
  fprintf (out, "Sum: %qd\n", stat->sum);
  fprintf (out, "Mean: %0.2f\n", (double) stat->sum / (double) stat->count);
  fprintf (out, "Stddev: %0.2f\n", stat_int_stddev (stat));

  if (fclose (out) < 0)
    g_error ("fclose failed");

  stat_int_histogram (stat);
}

/* Dbl stat
 */

DblStat*
stat_dbl_new (const char* name)
{
  DblStat* s = g_new0 (DblStat, 1);

  s->name = name;
  s->values = g_array_new (FALSE, FALSE, sizeof (double));

  return s;
}

void
stat_dbl_add_item (DblStat* stat, double v)
{
  if (! stat->count)
    stat->min = v;
  stat->count += 1;
  stat->min = MIN (v, stat->min);
  stat->max = MAX (v, stat->max);
  stat->sum += v;

  g_array_append_val (stat->values, v);
}

double
stat_dbl_stddev (DblStat *stat)
{
  double f = 0;
  double m = stat->sum / stat->count;
  double v;
  int i;

  for (i = 0; i < stat->count; i += 1)
    {
      double x = g_array_index (stat->values, double, i);

      f += (m - x) * (m - x);
    }

  v = f / stat->count;

  return sqrt (v);
}

int
dbl_comp (const void* a, const void* b)
{
  const double* da = a;
  const double* db = b;
  double diff = (*da) - (*db);

  if (diff > 0.0)
    return 1;
  else if (diff < 0.0)
    return -1;
  else
    return 0;
}

void
stat_dbl_histogram (DblStat *stat)
{
  int i, consec;
  double cum = 0.0;

  FILE* p_out;
  FILE* s_out;

  if (! (p_out = config_output ("%s.pop.hist", stat->name)))
    abort ();

  if (! (s_out = config_output ("%s.sum.hist", stat->name)))
    abort ();

  qsort (stat->values->data, stat->count, sizeof (double), dbl_comp);

  for (i = 0; i < stat->count; i += consec)
    {
      double ix = g_array_index (stat->values, double, i);

      for (consec = 1; (i+consec) < stat->count; consec += 1)
	{
	  double jx = g_array_index (stat->values, double, i+consec);

	  if (ix != jx)
	    break;
	}

      cum += ((double) consec) * g_array_index (stat->values, double, i);

      fprintf (p_out, "%0.6f, %0.3f\n", g_array_index (stat->values, double, i), (double) (i+consec) / (double) stat->count);
      fprintf (s_out, "%0.6f, %0.3f\n", g_array_index (stat->values, double, i), cum / stat->sum);
    }

  if (fclose (p_out) < 0 || fclose (s_out) < 0)
    {
      g_error ("fclose failed\n");
    }
}

void
stat_dbl_report (DblStat* stat)
{
  FILE* out;

  if (! (out = config_output ("%s.stat", stat->name)))
    abort ();

  fprintf (out, "Name:   %s\n", stat->name);
  fprintf (out, "Count:  %d\n", stat->count);
  fprintf (out, "Min:    %0.6f\n", stat->min);
  fprintf (out, "Max:    %0.6f\n", stat->max);
  fprintf (out, "Sum:    %0.6f\n", stat->sum);
  fprintf (out, "Mean:   %0.6f\n", stat->sum / stat->count);
  fprintf (out, "Stddev: %0.6f\n", stat_dbl_stddev (stat));

  if (fclose (out) < 0)
    g_error ("fclose failed");

  stat_dbl_histogram (stat);
}

/* Bincount
 */
BinCounter*
stat_bincount_new (const char* name)
{
  BinCounter* bc = g_new0 (BinCounter, 1);

  bc->name = name;
  bc->bins = g_ptr_array_new ();

  return bc;
}

void
stat_bincount_add_item (BinCounter* bc, int bin, double val)
{
  GArray* one;
  int last;

  if (bin >= bc->bins->len)
    {
      g_ptr_array_set_size (bc->bins, bin+1);
    }

  if (! (one = bc->bins->pdata[bin]))
    {
      one = bc->bins->pdata[bin] = g_array_new (FALSE, TRUE, sizeof (double));
    }

  g_assert (one);

  last = one->len;

  g_array_set_size (one, last + 1);

  g_array_index (one, double, last) = val;
}

void
stat_bincount_report (BinCounter* bc)
{
  FILE *avg_out;
  FILE *raw_out;
  int i;

  if (! (avg_out = config_output ("%s.avg", bc->name)))
    abort ();

  if (! (raw_out = config_output ("%s.raw", bc->name)))
    abort ();

  for (i = 0; i < bc->bins->len; i += 1)
    {
      GArray* one = bc->bins->pdata[i];

      double sum = 0.0;
      int j;

      for (j = 0; j < one->len; j += 1)
	{
	  double d = g_array_index (one, double, j);

	  sum += d;

	  fprintf (raw_out, "%e ", d);
	}

      fprintf (raw_out, "\n");
      fprintf (avg_out, "%e %d\n", sum / one->len, one->len);
    }

  if (fclose (avg_out) < 0)
    g_error ("fclose failed");

  if (fclose (raw_out) < 0)
    g_error ("fclose failed");
}

/* Config stuff
 */

int
config_create_dir (const char* dirname)
{
  struct stat buf;

  if (stat (dirname, & buf) < 0)
    {
      if (mkdir (dirname, 0777) < 0)
	{
	  fprintf (stderr, "mkdir failed: %s\n", dirname);
	  return errno;
	}
    }
  else
    {
      if (! S_ISDIR (buf.st_mode))
	{
	  fprintf (stderr, "not a directory: %s\n", dirname);
	  return errno;
	}
    }

  return 0;
}

int
config_clear_dir (const char* dir)
{
  char buf[1024];

  if (dir)
    {
      sprintf (buf, "rm -rf %s", dir);

      system (buf);
    }

  return 0;
}

static ConfigOption all_options[64];
static int          option_count;

void
config_init ()
{
  static gboolean once = FALSE;
  if (! once)
    {
      once = TRUE;
      config_register (config_options, ARRAY_SIZE (config_options));
    }
}

void
config_register (ConfigOption *opts, int nopts)
{
  int i;

  config_init ();

  for (i = 0; i < nopts; i += 1)
    {
      all_options[option_count++] = opts[i];
    }
}

void
config_set_string (const char* var, const char* val)
{
  int i;

  for (i = 0; i < option_count; i += 1)
    {
      ConfigOption *opt = all_options + i;

      if (strcmp (opt->name, var) == 0)
	{
	  (* (const char**) opt->value) = val;
	  opt->found = TRUE;
	  return;
	}
    }
}

int
config_parse (const char* config_file)
{
  FILE *in;
  char oname[1024], value[1024];
  int i;

  if (! (in = fopen (config_file, "r")))
    {
      fprintf (stderr, "fopen failed: %s\n", config_file);
      return errno;
    }

  for (;;)
    {
      ConfigOption *opt = NULL;

      if (fscanf (in, "%s", oname) != 1)
	break;

      for (i = 0; i < option_count; i += 1)
	{
	  if (strcmp (oname, all_options[i].name) == 0)
	    {
	      opt = all_options + i;
	      break;
	    }
	}

      if (opt && opt->arg == CO_None)
	{
	  (* (gboolean*) opt->value) = TRUE;
	  opt->found = TRUE;
	  continue;
	}

      if (fscanf (in, "%s", value) != 1)
	{
	  fprintf (stderr, "no value for option: %s; file: %s\n", oname, config_file);
	  goto abort;
	}

      if (! opt)
	{
	  /*fprintf (stderr, "unrecognized option: %s\n", oname);*/
	  continue;
	}

      switch (opt->type)
	{
	case CD_Bool:

	  if (strcasecmp (value, "yes") == 0 ||
	      strcasecmp (value, "true") == 0 ||
	      strcmp     (value, "1") == 0 ||
	      strcasecmp (value, "on") == 0)
	    {
	      ((gboolean*) opt->value) = TRUE;
	    }
	  else
	    {
	      ((gboolean*) opt->value) = FALSE;
	    }

	  break;
	case CD_Int32:

	  if (sscanf (value, "%d", (gint32*) opt->value) != 1)
	    {
	      fprintf (stderr, "parse error for option: %s; file: %s\n", oname, config_file);
	      goto abort;
	    }

	  break;
	case CD_Double:

	  if (sscanf (value, "%lf", (double*) opt->value) != 1)
	    {
	      fprintf (stderr, "parse error for option: %s; file: %s\n", oname, config_file);
	      goto abort;
	    }

	  break;
	case CD_String:

	  (* (const char**) opt->value) = g_strdup (value);

	  break;
	}

      opt->found = TRUE;
    }

  fclose (in);

  return 0;

 abort:

  fclose (in);

  return -1;
}

int
config_compute_output_dir ()
{
  char tmp[1024];
  char buf[1024];
  int i;
  gboolean last = FALSE;

  buf[0] = 0;

  for (i = 0; i < option_count; i += 1)
    {
      ConfigOption *opt = all_options + i;

      if (opt->style == CS_Ignore)
	continue;

      if (! opt->found)
	continue;

      if (last)
	strcat (buf, ",");

      last = TRUE;

      strcat (buf, opt->abbrev);
      strcat (buf, "=");

      switch (opt->type)
	{
	case CD_Bool:

	  if (* (gboolean*) opt->value)
	    strcat (buf, "true");
	  else
	    strcat (buf, "false");

	  break;
	case CD_Int32:

	  sprintf (tmp, "%d", (* (gint32*) opt->value));
	  strcat (buf, tmp);

	  break;
	case CD_Double:

	  sprintf (tmp, "%0.2f", (* (double*) opt->value));
	  strcat (buf, tmp);

	  break;
	case CD_String:

	  if (opt->style == CS_UseAsFile)
	    {
	      const char* str = (* (const char**) opt->value);
	      const char* ls = strrchr (str, '/');

	      strcat (buf, ls ? (ls + 1) : str);
	    }
	  else
	    {
	      strcat (buf, (* (const char**) opt->value));
	    }

	  break;
	}
    }

  config_output_dir = g_strdup_printf ("%s/%s", config_output_base, buf);

  return 0;
}

int
config_done (void)
{
  int i, ret;
  FILE *out;

  for (i = 0; i < option_count; i += 1)
    {
      ConfigOption *opt = all_options + i;

      if (! opt->found && opt->arg == CO_Required)
	{
	  fprintf (stderr, "required option not found: %s\n", all_options[i].name);
	  return -1;
	}
    }

  if ((ret = config_compute_output_dir ())) {
    return ret;
  }

  if ((ret = config_clear_dir (config_output_dir))) {
    return ret;
  }

  if ((ret = config_create_dir (config_output_dir))) {
    return ret;
  }

  if (! (out = config_output ("Options")))
    abort ();

  for (i = 0; i < option_count; i += 1)
    {
      ConfigOption *opt = all_options + i;

      fprintf (out, "option: %s; value: ", all_options[i].name);

      switch (opt->type)
	{
	case CD_Bool:

	  fprintf (out, "%s", (* (gboolean*) opt->value) ? "TRUE" : "FALSE");

	  break;
	case CD_Int32:

	  fprintf (out, "%d", (* (gint32*) opt->value));

	  break;
	case CD_Double:

	  fprintf (out, "%0.2f", (* (double*) opt->value));

	  break;
	case CD_String:

	  fprintf (out, "%s", (* (const char**) opt->value));

	  break;
	}

      fprintf (out, "\n");
    }

  if (fclose (out))
    {
      fprintf (stderr, "fclose failed\n");
      return errno;
    }

  return 0;
}

const char*
config_help_arg (ConfigOption *opt)
{
  switch (opt->arg)
    {
    case CO_Required:
      return "required";
    case CO_Optional:
      return "optional";
    case CO_None:
      return "no value";
    }

  return "unknown";
}

const char*
config_help_type (ConfigOption *opt)
{
  switch (opt->arg)
    {
    case CO_None:
      return "boolean";
    default:
      break;
    }

  switch (opt->type)
    {
    case CD_Bool:
      return "boolean";
    case CD_Int32:
      return "int";
    case CD_Double:
      return "double";
    case CD_String:
      return "string";
    }

  return "unknown";
}

void
config_help (void)
{
  int i;

  fprintf (stderr, "Expecting the following options in one or more config files on the command line:\n");

  for (i = 0; i < option_count; i += 1)
    {
      ConfigOption *opt = all_options + i;

      fprintf (stderr, "%s: %s %s\n",
	       opt->name,
	       config_help_arg (opt),
	       config_help_type (opt));
    }
}

FILE*
config_output (const char* format, ...)
{
  gchar *buffer;
  gchar *file;
  va_list args;
  FILE *f;

  va_start (args, format);
  buffer = g_strdup_vprintf (format, args);
  va_end (args);

  file = g_strdup_printf ("%s/%s", config_output_dir, buffer);

  if (! (f = fopen (file, "w")))
    g_error ("fopen failed: %s\n", buffer);

  g_free (file);

  g_free (buffer);

  return f;
}


#include <edsio.h>
#include <edsiostdio.h>
#include <ctype.h>
#include "xdfs.h"

/* Warning: very cheesy!
 */

#ifdef DEBUG_EXTRACT
  FileHandle *fh2 = handle_read_file (filename);

  guint8* debug_buf = g_malloc (buflen);

  if (! handle_read (fh2, debug_buf, buflen))
    g_error ("read failed");
#endif

gboolean
rcs_count (const char* filename, guint *encoded_size)
{
  char *readbuf0, *readbuf;
  gboolean in_string = FALSE;
  gboolean in_text = FALSE;
  guint string_start = 0;
  guint string_end = 0;
  guint current_pos = 0;
  /*char *current_delta = NULL;*/
  FileHandle *fh = handle_read_file (filename);
  guint buflen = handle_length (fh);

  (* encoded_size) = 0;

  readbuf0 = g_new (guint8, buflen);

  for (;;)
    {
      int c = handle_gets (fh, readbuf0, buflen);

      readbuf = readbuf0;

      if (c < 0)
	break;

      if (strncmp (readbuf, "text", 4) == 0)
	in_text = TRUE;

      if (! in_string && readbuf[0] == '@')
	{
	  string_start = current_pos + 1;
	  in_string = TRUE;
	  readbuf += 1;
	}

      current_pos += c;

      if (in_string)
	{
	  while ((readbuf = strchr (readbuf, '@')))
	    {
	      if (readbuf[1] == '@')
		{
		  string_start += 1; /* @@@ bogus, just counting. */
		  readbuf += 2;
		  continue;
		}

	      in_string = FALSE;
	      break;
	    }

	  string_end = current_pos - 2;

	  if (in_text && ! in_string)
	    {
	      in_text = FALSE;

	      /*g_free (current_delta);
		current_delta = NULL;*/

	      (* encoded_size) += (string_end - string_start);
	    }

	  continue;
	}

      if (isdigit (readbuf[0]))
	{
#if 0
	  (* strchr (readbuf, '\n')) = 0;
	  if (current_delta)
	    g_free (current_delta);
	  current_delta = g_strdup (readbuf);
#endif
	}
    }

  handle_close (fh);

  g_free (readbuf0);

#if 0
  if (current_delta)
    g_free (current_delta);
#endif

  return TRUE;
}

#if 0
int
main (int argc, char** argv)
{
  guint size;

  if (argc != 2)
    g_error ("usage: %s RCS_file\n", argv[0]);

  if (! rcs_count (argv[1], &size))
    g_error ("rcs_parse failed");

  return 0;
}
#endif
