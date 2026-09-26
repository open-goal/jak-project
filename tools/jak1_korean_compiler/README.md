# Jak 1 Korean compiler

이 디렉터리는 Jak 1 한국어 폰트 생성과 게임 데이터 재컴파일에 필요한 로컬 실행 스크립트를 모은다. 프로젝트 루트는 이 디렉터리의 두 단계 상위다. 원본 ISO는 변경하지 않는다.

## 사용

프로젝트 루트에서 `./dev.ps1 doctor`, `./dev.ps1 compile`을 실행하거나 이 디렉터리에서 `./dev.ps1 compile` 또는 `compile.cmd`를 실행한다. `compile`은 폰트 자산을 갱신하고, 필요하면 텍스처를 다시 추출한 뒤 `17COMMON.TXT`, `17SUBTIT.TXT`, `GAME.CGO`를 재생성한다. `./dev.ps1 build`는 C++ 실행 파일을 먼저 빌드한다.

| 파일 | 역할 |
| --- | --- |
| `dev.ps1`, `compile.cmd` | 진단·설정·빌드·폰트 생성·추출·컴파일 실행 |
| `dev-env.ps1` | 현재 PowerShell 프로세스에 MSVC 및 로컬 도구 경로 설정 |
| `build-jak1-korean-font.py` | 자모 아틀라스, 글리프 매핑, GOAL 폰트 테이블 생성 |
| `extract-iso.py`, `verify-jak1-disc.py` | ISO 파일 추출 및 현재 SCPS-56003 디스크 검증 |
| `requirements-dev.txt` | Python 패키지 버전 고정 |

`.venv/`, `deps/`, `bin/`, `.tmp/`, `.cache/`는 이 PC의 로컬 도구이며 Git에서 제외한다. `.venv/`에는 Python과 필요한 패키지가, `deps/`에는 Task와 NASM이 있다. 이동한 가상환경의 `cmake.exe` 진입점은 이전 경로를 기억할 수 있어 빌드 스크립트는 `python -m cmake`를 사용한다. 다른 PC에서는 Python 가상환경을 새로 만들고 `requirements-dev.txt`를 설치해야 한다. NASM과 MSVC Build Tools는 별도 설치가 필요하다.

프로젝트에서 빌드한 `goalc.exe`, `extractor.exe`, `gk.exe`와 DLL의 원본은 CMake가 관리하는 `out/build/Release/bin/`에 남겨 둔다. 원본을 옮기면 다음 CMake 빌드와 실행 경로가 어긋나므로 `dev.ps1`이 최신 실행 파일과 DLL을 이 디렉터리의 `bin/`으로 복사한 뒤 사용한다. `bin/`은 Git 제외 대상이다. 등록된 MSVC Build Tools는 `.tools/vs-buildtools/`에 유지한다. `.venv/`와 `deps/` 및 `out/`은 Git에 포함되지 않으므로 이 저장소만 복제한 PC에는 실행 파일이 자동으로 제공되지 않는다.

폰트 생성 입력은 `scripts/jamos.png`, `game/assets/fonts/jak2_jak3_korean_db.json`, 한국어 번역 JSON, 추출된 원본 폰트 텍스처다. 생성 결과는 `custom_assets/jak1/texture_replacements/gamefontnew/*.png`, `game/assets/jak1/korean_glyph_map.json`, `goal_src/jak1/engine/gfx/korean-font.gc`에 저장한다. 이 결과물은 Git으로 관리한다.
