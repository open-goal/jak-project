import json
import argparse
import re

# script to generate dummy subtitle2 JSON entries for a given speaker
# usage: python subtitle2-gen.py <path/to/vag_list> <speaker>
# to get the vag list, run (vag-list-to-file "file-name") in the REPL

# speaker regexes
speakers = {
    "computer": "cityv[0-9]{3}$",
    "jak": "(jak|jk|jd)[0-9]{3}$",
    "darkjak": "",
    "daxter": "(ds|dsek|dsbop)[0-9]{3}$",
    "samos": "sam[0-9]{3}$",
    "keira": "kei[0-9]{3}$",
    "keira-before-class-3": "kei[0-9]{3}$",
    "kid": "",
    "kor": "kor[0-9]{3}$",
    "metalkor": "",
    "baron": "(bf|bar|prop)[0-9]{3}$",
    "errol": "ero[0-9]{3}$",
    "torn": "(bb|tor|torn)[0-9]{3}$",
    "tess": "(tess|tswm)[0-9]{3}$",
    "guard": "kgv?[0-9]{3}$",
    "guard-a": "kg[0-9]{3}a",
    "guard-b": "kg[0-9]{3}b",
    "krew": "(krew|kwbf)[0-9]{3}$",
    "sig": "(sigt|sigc)[0-9]{3}$",
    "brutter": "bru[0-9]{3}$",
    "vin": "vin[0-9]{3}$",
    "youngsamos": "ys[0-9]{3}$",
    "youngsamos-before-rescue": "",
    "pecker": "pek[0-9]{3}$",
    "onin": "",
    "ashelin": "asha[0-9]{3}$",
    "jinx": "hal[0-9]{3}$",
    "mog": "hal[0-9]{3}$",
    "grim": "hal[0-9]{3}$",
    "agent": "agnt[0-9]{3}$",
    # there's no clear pattern for what citizen line is male or female, but this seems to be the closest match
    "citizen-male": "cit[0-9]{3}(a|b)?$",
    "citizen-female": "cit[0-9]{3}(c|d)$",
    "oracle": "ora[0-9]{3}$",
    "precursor": "",
}

vag_list = []

sub_json = {}

def read_vag_list(file_name):
    with open(file_name) as f:
        for line in f:
            vag_list.append(line.strip())

def gen_json_for_speaker(speaker):
    sub = {
        "lines": [
            {
                "end": 10000.0,
                "merge": False,
                "offscreen": True,
                "speaker": speaker,
                "start": 0.0,
                "text": "TODO",
            }
        ]
    }
    return sub

def get_vags_for_speaker(speaker):
    vags = []
    names = speakers[speaker]
    if not names:
        return []
    else:
        reg = re.compile(names)
        result = list(filter(reg.match, vag_list))
        vags.extend(result)
    return vags

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("vag_list", type=str)
    parser.add_argument("speaker", type=str)
    args = parser.parse_args()

    read_vag_list(args.vag_list)
    try:
        vags = get_vags_for_speaker(args.speaker)
    except KeyError:
        print("No vags found for speaker " + args.speaker)
        exit(1)
    for vag in vags:
        sub_json[vag] = (gen_json_for_speaker(args.speaker))
    with open (args.speaker + "_subs.json", "w") as f:
        json.dump(sub_json, f, indent=2)

if __name__ == "__main__":
    main()