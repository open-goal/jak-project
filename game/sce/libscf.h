#ifndef JAK1_LIBSCF_H
#define JAK1_LIBSCF_H

#define SCE_JAPANESE_LANGUAGE 0
#define SCE_ENGLISH_LANGUAGE 1
#define SCE_FRENCH_LANGUAGE 2
#define SCE_SPANISH_LANGUAGE 3
#define SCE_GERMAN_LANGUAGE 4
#define SCE_ITALIAN_LANGUAGE 5
#define SCE_DUTCH_LANGUAGE 6
#define SCE_PORTUGUESE_LANGUAGE 7

#define SCE_ASPECT_43 0
#define SCE_ASPECT_FULL 1
#define SCE_ASPECT_169 2

namespace ee {
/*!
 * Get the aspect ratio setting of the PS2.
 * It is either 4:3, 16:9, or FULL.
 */
int sceScfGetAspect();

/*!
 * Get the language setting of the PS2.
 * Return a SONY SCE_LANGUAGE value, which differs from GOAL.
 */
int sceScfGetLanguage();
}  // namespace ee

#endif  // JAK1_LIBSCF_H
