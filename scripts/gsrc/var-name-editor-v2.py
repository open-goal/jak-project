#!/usr/bin/env python3
"""Qt editor for decompiler var_names and method docstrings."""

import argparse
import bisect
import json
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

from PyQt5 import QtCore, QtGui, QtWidgets


DEFAULT_VAR_NAMES = Path("decompiler/config/jak1/ntsc_v1/var_names.jsonc")
DEFAULT_METHOD_DOCS = Path("docs/jak1_cleanup_inputs/method_names.jsonc")

FUNCTION_COMMENT_RE = re.compile(
    r"^;; definition (?:\(debug\) )?for (?:function|behavior) (?P<name>.+?)\s*$"
)
METHOD_COMMENT_RE = re.compile(r"^;; definition for method (?P<id>\d+) of type (?P<type>\S+)\s*$")
DEF_HEADER_RE = re.compile(r"\((?:defun|defun-debug|defmethod|defbehavior)\s+(?P<name>\S+)")
STATE_HEADER_RE = re.compile(
    r"^[ \t]*\(\s*defstate\s+(?P<state>\S+)\s+\(\s*(?P<type>\S+)\s*\)", re.MULTILINE
)
BEHAVIOR_HEADER_RE = re.compile(r"\(\s*behavior\b")
LOCAL_VAR_RE = re.compile(r"\b(?:(?:[avts]\d)|(?:f\d+)|(?:sv\d*)|gp|sp|fp|ra|r0|pp)-\d+\b")
STATE_HANDLER_NAMES = ("event", "enter", "post", "trans", "exit", "code")


@dataclass
class DefinitionBlock:
    key: str
    kind: str
    display_name: str
    text: str
    start_line: int
    end_line: int
    method_id: int | None = None
    type_name: str | None = None
    args: list[str] = field(default_factory=list)

    @property
    def label(self) -> str:
        return f"{self.kind:8} {self.key}  ({self.start_line}-{self.end_line})"


def strip_jsonc(text: str) -> str:
    out = []
    i = 0
    in_string = False
    escaped = False

    while i < len(text):
        char = text[i]
        if in_string:
            out.append(char)
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            i += 1
            continue

        if char == '"':
            in_string = True
            out.append(char)
            i += 1
            continue

        if char == "/" and i + 1 < len(text) and text[i + 1] == "/":
            i += 2
            while i < len(text) and text[i] not in "\r\n":
                i += 1
            continue

        if char == "/" and i + 1 < len(text) and text[i + 1] == "*":
            i += 2
            while i + 1 < len(text) and not (text[i] == "*" and text[i + 1] == "/"):
                i += 1
            i += 2
            continue

        out.append(char)
        i += 1

    return "".join(out)


def remove_trailing_commas(text: str) -> str:
    out = []
    i = 0
    in_string = False
    escaped = False

    while i < len(text):
        char = text[i]
        if in_string:
            out.append(char)
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            i += 1
            continue

        if char == '"':
            in_string = True
            out.append(char)
            i += 1
            continue

        if char == ",":
            j = i + 1
            while j < len(text) and text[j].isspace():
                j += 1
            if j < len(text) and text[j] in "}]":
                i += 1
                continue

        out.append(char)
        i += 1

    return "".join(out)


def load_jsonc(path: Path) -> dict:
    if not path or not path.exists():
        return {}
    return json.loads(remove_trailing_commas(strip_jsonc(path.read_text())))


def save_json(path: Path, data: dict) -> None:
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")


def monospace_font() -> QtGui.QFont:
    font = QtGui.QFont("monospace")
    font.setStyleHint(QtGui.QFont.Monospace)
    return font


def sexpr_text_from_header(text: str) -> str:
    start = text.find("(def")
    if start < 0:
        return ""

    depth = 0
    in_string = False
    escaped = False
    for idx, char in enumerate(text[start:], start):
        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue

        if char == '"':
            in_string = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return text[start : idx + 1]
    return ""


def first_list_after_def_name(header: str) -> str:
    if not header:
        return ""
    header_match = DEF_HEADER_RE.search(header)
    if not header_match:
        return ""
    start = header.find("(", header_match.end())
    if start < 0:
        return ""

    depth = 0
    in_string = False
    escaped = False
    for idx, char in enumerate(header[start:], start):
        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue

        if char == '"':
            in_string = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return header[start : idx + 1]
    return ""


def parse_arg_names_from_list(arg_list: str) -> list[str]:
    if len(arg_list) < 2:
        return []

    args = []
    inner = arg_list[1:-1]
    depth = 0
    start = None
    bare_token_start = None
    for idx, char in enumerate(inner):
        if char == "(":
            if depth == 0 and bare_token_start is not None:
                args.append(inner[bare_token_start:idx].strip())
                bare_token_start = None
            if depth == 0:
                start = idx
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0 and start is not None:
                arg_expr = inner[start : idx + 1]
                arg_match = re.match(r"\(\s*(\S+)", arg_expr)
                if arg_match:
                    args.append(arg_match.group(1))
                start = None
        elif depth == 0 and char.isspace():
            if bare_token_start is not None:
                args.append(inner[bare_token_start:idx].strip())
                bare_token_start = None
        elif depth == 0 and bare_token_start is None:
            bare_token_start = idx

    if bare_token_start is not None:
        args.append(inner[bare_token_start:].strip())

    return [arg for arg in args if arg]


def parse_args_from_header(header: str) -> list[str]:
    return parse_arg_names_from_list(first_list_after_def_name(header))


def first_list_after_behavior_name(header: str) -> str:
    if not header:
        return ""
    header_match = BEHAVIOR_HEADER_RE.search(header)
    if not header_match:
        return ""
    start = header.find("(", header_match.end())
    if start < 0:
        return ""

    depth = 0
    in_string = False
    escaped = False
    for idx, char in enumerate(header[start:], start):
        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue

        if char == '"':
            in_string = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return header[start : idx + 1]
    return ""


def parse_args_from_behavior(header: str) -> list[str]:
    return parse_arg_names_from_list(first_list_after_behavior_name(header))


def line_start_offsets(text: str) -> list[int]:
    starts = [0]
    for idx, char in enumerate(text):
        if char == "\n":
            starts.append(idx + 1)
    return starts


def line_number_for_offset(starts: list[int], offset: int) -> int:
    return bisect.bisect_right(starts, offset)


def skip_ws_and_line_comments(text: str, pos: int, end: int | None = None) -> int:
    if end is None:
        end = len(text)
    while pos < end:
        if text[pos].isspace():
            pos += 1
            continue
        if text[pos] == ";":
            newline = text.find("\n", pos, end)
            if newline < 0:
                return end
            pos = newline + 1
            continue
        break
    return pos


def find_matching_paren(text: str, open_idx: int, limit: int | None = None) -> int:
    if limit is None:
        limit = len(text)

    depth = 0
    in_string = False
    escaped = False
    in_line_comment = False
    idx = open_idx
    while idx < limit:
        char = text[idx]
        if in_line_comment:
            if char == "\n":
                in_line_comment = False
            idx += 1
            continue

        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            idx += 1
            continue

        if char == ";":
            in_line_comment = True
        elif char == '"':
            in_string = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return idx
        idx += 1

    return -1


def read_expr_range(text: str, pos: int, limit: int) -> tuple[int, int] | None:
    pos = skip_ws_and_line_comments(text, pos, limit)
    if pos >= limit:
        return None

    if text[pos] == "(":
        end = find_matching_paren(text, pos, limit)
        if end < 0:
            return None
        return pos, end + 1

    end = pos
    while end < limit and not text[end].isspace() and text[end] not in "()":
        end += 1
    return pos, end


def iter_state_handler_exprs(text: str, form_start: int, form_end: int):
    depth = 1
    in_string = False
    escaped = False
    in_line_comment = False
    idx = form_start + 1

    while idx < form_end:
        char = text[idx]
        if in_line_comment:
            if char == "\n":
                in_line_comment = False
            idx += 1
            continue

        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            idx += 1
            continue

        if char == ";":
            in_line_comment = True
            idx += 1
            continue
        if char == '"':
            in_string = True
            idx += 1
            continue

        if depth == 1 and char == ":":
            for handler in STATE_HANDLER_NAMES:
                keyword = f":{handler}"
                keyword_end = idx + len(keyword)
                if text.startswith(keyword, idx) and (
                    keyword_end >= form_end or text[keyword_end].isspace() or text[keyword_end] in "()"
                ):
                    expr_range = read_expr_range(text, keyword_end, form_end)
                    if expr_range:
                        expr_start, expr_end = expr_range
                        yield handler, idx, expr_start, expr_end
                        idx = expr_end
                    break
            else:
                idx += 1
            continue

        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        idx += 1


def parse_state_blocks(path: Path) -> list[DefinitionBlock]:
    text = path.read_text()
    starts = line_start_offsets(text)
    blocks = []

    for match in STATE_HEADER_RE.finditer(text):
        form_start = match.start()
        form_end = find_matching_paren(text, form_start)
        if form_end < 0:
            continue

        state_name = match.group("state")
        type_name = match.group("type")
        for handler, keyword_start, expr_start, expr_end in iter_state_handler_exprs(text, form_start, form_end):
            expr_text = text[expr_start:expr_end]
            if not BEHAVIOR_HEADER_RE.match(expr_text.lstrip()):
                continue

            block_text = f";; state {handler} for {state_name} ({type_name})\n{text[keyword_start:expr_end]}\n"
            key = f"({handler} {state_name} {type_name})"
            blocks.append(
                DefinitionBlock(
                    key=key,
                    kind=f"state-{handler}",
                    display_name=f"{handler} {state_name} {type_name}",
                    text=block_text,
                    start_line=line_number_for_offset(starts, keyword_start),
                    end_line=line_number_for_offset(starts, expr_end),
                    type_name=type_name,
                    args=parse_args_from_behavior(expr_text),
                )
            )

    return blocks


def parse_display_name(header: str, fallback: str) -> str:
    match = DEF_HEADER_RE.search(header)
    return match.group("name") if match else fallback


def parse_disasm(path: Path) -> list[DefinitionBlock]:
    lines = path.read_text().splitlines()
    starts = []

    for idx, line in enumerate(lines):
        function_match = FUNCTION_COMMENT_RE.match(line)
        if function_match:
            name = function_match.group("name")
            starts.append((idx, "function", name, None, None))
            continue

        method_match = METHOD_COMMENT_RE.match(line)
        if method_match:
            method_id = int(method_match.group("id"))
            type_name = method_match.group("type")
            key = f"(method {method_id} {type_name})"
            starts.append((idx, "method", key, method_id, type_name))

    blocks = []
    for start_idx, (line_idx, kind, key, method_id, type_name) in enumerate(starts):
        end_idx = starts[start_idx + 1][0] if start_idx + 1 < len(starts) else len(lines)
        text = "\n".join(lines[line_idx:end_idx])
        if end_idx > line_idx:
            text += "\n"
        header = sexpr_text_from_header(text)
        blocks.append(
            DefinitionBlock(
                key=key,
                kind=kind,
                display_name=parse_display_name(header, key),
                text=text,
                start_line=line_idx + 1,
                end_line=end_idx,
                method_id=method_id,
                type_name=type_name,
                args=parse_args_from_header(header),
            )
        )

    blocks.extend(parse_state_blocks(path))
    blocks.sort(key=lambda block: (block.start_line, block.end_line, block.kind))
    return blocks


def get_local_override_name(value):
    if isinstance(value, str):
        return value
    if isinstance(value, list) and value:
        return value[0]
    return None


def set_local_override(entry: dict, var_id: str, new_name: str) -> None:
    vars_entry = entry.setdefault("vars", {})
    existing = vars_entry.get(var_id)
    if isinstance(existing, list) and len(existing) >= 2:
        updated = list(existing)
        updated[0] = new_name
        vars_entry[var_id] = updated
    else:
        vars_entry[var_id] = new_name


class DefinitionTextEdit(QtWidgets.QTextEdit):
    clicked_at = QtCore.pyqtSignal(QtCore.QPoint)

    def mousePressEvent(self, event):
        super().mousePressEvent(event)
        self.clicked_at.emit(event.pos())


class ColumnGuideTextEdit(QtWidgets.QTextEdit):
    def __init__(self, column: int, parent=None):
        super().__init__(parent)
        self.column = column
        self.setFont(monospace_font())
        self.setLineWrapMode(QtWidgets.QTextEdit.NoWrap)

    def paintEvent(self, event):
        super().paintEvent(event)
        font_metrics = QtGui.QFontMetrics(self.font())
        column_x = (
            self.document().documentMargin()
            + font_metrics.horizontalAdvance(" ") * self.column
            - self.horizontalScrollBar().value()
        )
        if column_x < 0 or column_x > self.viewport().width():
            return

        painter = QtGui.QPainter(self.viewport())
        pen = QtGui.QPen(QtGui.QColor("#d1d5db"))
        pen.setWidth(1)
        painter.setPen(pen)
        x = int(column_x)
        painter.drawLine(x, 0, x, self.viewport().height())


class VarNameEditor(QtWidgets.QMainWindow):
    def __init__(self, args):
        super().__init__()
        self.setWindowTitle("OpenGOAL Variable Name Editor")
        self.resize(1320, 840)

        self.disasm_path = Path(args.disasm).resolve() if args.disasm else None
        self.var_names_path = Path(args.var_names).resolve() if args.var_names else DEFAULT_VAR_NAMES.resolve()
        self.method_docs_path = (
            Path(args.method_docs).resolve() if args.method_docs else DEFAULT_METHOD_DOCS.resolve()
        )

        self.blocks: list[DefinitionBlock] = []
        self.filtered_blocks: list[DefinitionBlock] = []
        self.current_block: DefinitionBlock | None = None
        self.var_names = {}
        self.method_docs = {}
        self.disasm_files: list[Path] = []
        self.filtered_disasm_files: list[Path] = []
        self.token_ranges = []
        self.selected_token = None
        self.dirty_vars = False
        self.dirty_docs = False

        self._build_ui()
        self._load_initial_files()

    def _build_ui(self) -> None:
        root = QtWidgets.QWidget()
        self.setCentralWidget(root)
        layout = QtWidgets.QVBoxLayout(root)

        toolbar = QtWidgets.QHBoxLayout()
        layout.addLayout(toolbar)

        open_disasm = QtWidgets.QPushButton("Open Disasm")
        open_disasm.clicked.connect(self.open_disasm)
        toolbar.addWidget(open_disasm)
        self.disasm_edit = QtWidgets.QLineEdit()
        toolbar.addWidget(self.disasm_edit, 2)

        load_vars = QtWidgets.QPushButton("Load Vars")
        load_vars.clicked.connect(self.load_var_names_dialog)
        toolbar.addWidget(load_vars)
        self.var_path_edit = QtWidgets.QLineEdit()
        toolbar.addWidget(self.var_path_edit, 2)
        save_vars = QtWidgets.QPushButton("Save Vars")
        save_vars.clicked.connect(self.save_var_names)
        toolbar.addWidget(save_vars)

        load_docs = QtWidgets.QPushButton("Load Docs")
        load_docs.clicked.connect(self.load_method_docs_dialog)
        toolbar.addWidget(load_docs)
        self.docs_path_edit = QtWidgets.QLineEdit()
        toolbar.addWidget(self.docs_path_edit, 2)
        save_docs = QtWidgets.QPushButton("Save Docs")
        save_docs.clicked.connect(self.save_method_docs)
        toolbar.addWidget(save_docs)

        filter_layout = QtWidgets.QHBoxLayout()
        layout.addLayout(filter_layout)
        filter_layout.addWidget(QtWidgets.QLabel("Definition Filter"))
        self.filter_edit = QtWidgets.QLineEdit()
        self.filter_edit.textChanged.connect(self.refresh_definition_list)
        filter_layout.addWidget(self.filter_edit)

        splitter = QtWidgets.QSplitter(QtCore.Qt.Horizontal)
        layout.addWidget(splitter, 1)

        file_panel = QtWidgets.QWidget()
        file_layout = QtWidgets.QVBoxLayout(file_panel)
        file_layout.setContentsMargins(0, 0, 0, 0)
        file_layout.addWidget(QtWidgets.QLabel("File Filter"))
        self.file_filter_edit = QtWidgets.QLineEdit()
        self.file_filter_edit.setPlaceholderText("Partial filename")
        self.file_filter_edit.textChanged.connect(lambda _text: self.apply_file_filter())
        file_layout.addWidget(self.file_filter_edit)
        self.file_list = QtWidgets.QListWidget()
        self.file_list.currentRowChanged.connect(self.on_file_selected)
        file_layout.addWidget(self.file_list, 1)
        splitter.addWidget(file_panel)

        self.definition_list = QtWidgets.QListWidget()
        self.definition_list.currentRowChanged.connect(self.on_definition_selected)
        splitter.addWidget(self.definition_list)

        self.text = DefinitionTextEdit()
        self.text.setReadOnly(False)
        self.text.setLineWrapMode(QtWidgets.QTextEdit.NoWrap)
        self.text.setFont(monospace_font())
        self.text.clicked_at.connect(self.on_text_click)
        splitter.addWidget(self.text)
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 2)
        splitter.setStretchFactor(2, 6)

        bottom = QtWidgets.QGridLayout()
        layout.addLayout(bottom)
        bottom.addWidget(QtWidgets.QLabel("Selected"), 0, 0)
        self.selected_edit = QtWidgets.QLineEdit()
        self.selected_edit.setReadOnly(True)
        bottom.addWidget(self.selected_edit, 0, 1)
        bottom.addWidget(QtWidgets.QLabel("Rename To"), 0, 2)
        self.rename_edit = QtWidgets.QLineEdit()
        self.rename_edit.returnPressed.connect(self.apply_rename)
        bottom.addWidget(self.rename_edit, 0, 3)
        apply_rename = QtWidgets.QPushButton("Apply Rename")
        apply_rename.clicked.connect(self.apply_rename)
        bottom.addWidget(apply_rename, 0, 4)

        doc_group = QtWidgets.QGroupBox("Function/Method Docstring")
        doc_layout = QtWidgets.QHBoxLayout(doc_group)
        self.doc_text = ColumnGuideTextEdit(110)
        self.doc_text.setFixedHeight(90)
        doc_layout.addWidget(self.doc_text, 1)
        apply_doc = QtWidgets.QPushButton("Apply Docstring")
        apply_doc.clicked.connect(self.apply_docstring)
        doc_layout.addWidget(apply_doc)
        bottom.addWidget(doc_group, 1, 0, 1, 5)

        self.statusBar()

    def _load_initial_files(self) -> None:
        self.var_path_edit.setText(str(self.var_names_path))
        self.docs_path_edit.setText(str(self.method_docs_path))
        self.var_names = load_jsonc(self.var_names_path)
        self.method_docs = load_jsonc(self.method_docs_path)
        if self.disasm_path:
            self.load_disasm(self.disasm_path)
        else:
            self.set_status("Open a *_disasm.gc file to begin.")

    def set_status(self, text: str) -> None:
        self.statusBar().showMessage(text)

    def open_disasm(self) -> None:
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, "Open decompiler output", "", "GOAL disassembly (*_disasm.gc);;GOAL (*.gc);;All (*)"
        )
        if path:
            self.load_disasm(Path(path))

    def load_disasm(self, path: Path) -> None:
        self.disasm_path = path.resolve()
        self.disasm_edit.setText(str(self.disasm_path))
        self.refresh_file_list(self.disasm_path.parent, self.disasm_path)
        self.blocks = parse_disasm(self.disasm_path)
        self.refresh_definition_list()
        if self.blocks:
            self.definition_list.setCurrentRow(0)
        self.set_status(f"Loaded {len(self.blocks)} definitions from {self.disasm_path}")

    def refresh_file_list(self, directory: Path, current_path: Path | None = None) -> None:
        self.disasm_files = sorted(directory.glob("*_disasm.gc"))
        self.apply_file_filter(current_path)

    def apply_file_filter(self, current_path: Path | None = None) -> None:
        query = self.file_filter_edit.text().strip().lower()
        self.filtered_disasm_files = [
            path for path in self.disasm_files if not query or query in path.name.lower()
        ]
        self.file_list.blockSignals(True)
        self.file_list.clear()
        selected_row = -1
        active_path = current_path or self.disasm_path
        for index, path in enumerate(self.filtered_disasm_files):
            self.file_list.addItem(path.name)
            if active_path and path.resolve() == active_path.resolve():
                selected_row = index
        if selected_row >= 0:
            self.file_list.setCurrentRow(selected_row)
        self.file_list.blockSignals(False)

    def on_file_selected(self, row: int) -> None:
        if row < 0 or row >= len(self.filtered_disasm_files):
            return
        path = self.filtered_disasm_files[row]
        if self.disasm_path and path.resolve() == self.disasm_path.resolve():
            return
        self.load_disasm(path)

    def load_var_names_dialog(self) -> None:
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, "Load var_names.jsonc", "", "JSONC (*.jsonc);;JSON (*.json);;All (*)"
        )
        if path:
            self.var_names_path = Path(path).resolve()
            self.var_path_edit.setText(str(self.var_names_path))
            self.var_names = load_jsonc(self.var_names_path)
            self.dirty_vars = False
            self.render_current_block()
            self.set_status(f"Loaded variable names from {self.var_names_path}")

    def load_method_docs_dialog(self) -> None:
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, "Load method docstrings", "", "JSONC (*.jsonc);;JSON (*.json);;All (*)"
        )
        if path:
            self.method_docs_path = Path(path).resolve()
            self.docs_path_edit.setText(str(self.method_docs_path))
            self.method_docs = load_jsonc(self.method_docs_path)
            self.dirty_docs = False
            self.render_current_block()
            self.set_status(f"Loaded method docstrings from {self.method_docs_path}")

    def save_var_names(self) -> None:
        path = Path(self.var_path_edit.text()).expanduser()
        save_json(path, self.var_names)
        self.var_names_path = path.resolve()
        self.dirty_vars = False
        self.set_status(f"Saved variable names to {self.var_names_path}")

    def save_method_docs(self) -> None:
        path = Path(self.docs_path_edit.text()).expanduser()
        save_json(path, self.method_docs)
        self.method_docs_path = path.resolve()
        self.dirty_docs = False
        self.set_status(f"Saved method docstrings to {self.method_docs_path}")

    def refresh_definition_list(self) -> None:
        query = self.filter_edit.text().strip().lower()
        self.filtered_blocks = [
            block
            for block in self.blocks
            if not query or query in block.label.lower() or query in block.display_name.lower()
        ]
        self.definition_list.clear()
        for block in self.filtered_blocks:
            self.definition_list.addItem(block.label)

    def on_definition_selected(self, row: int) -> None:
        if row < 0 or row >= len(self.filtered_blocks):
            return
        self.current_block = self.filtered_blocks[row]
        self.selected_token = None
        self.selected_edit.clear()
        self.rename_edit.clear()
        self.render_current_block()

    def entry_for_current_block(self) -> dict:
        if not self.current_block:
            return {}
        return self.var_names.setdefault(self.current_block.key, {})

    def current_arg_names(self) -> list[str]:
        block = self.current_block
        if not block:
            return []
        entry = self.entry_for_current_block()
        configured = entry.get("args", [])
        result = []
        for index, original in enumerate(block.args):
            result.append(configured[index] if index < len(configured) else original)
        return result

    def current_docstring(self) -> str:
        block = self.current_block
        if not block:
            return ""
        entry = self.method_docs.get(block.key)
        if isinstance(entry, list) and len(entry) >= 2:
            return entry[1]
        return ""

    def render_current_block(self) -> None:
        block = self.current_block
        self.text.clear()
        self.doc_text.clear()
        self.token_ranges = []

        if not block:
            return

        self.doc_text.setEnabled(block.kind in ("function", "method"))
        if block.kind in ("function", "method"):
            self.doc_text.setPlainText(self.current_docstring())

        entry = self.entry_for_current_block()
        local_overrides = {
            key: get_local_override_name(value) for key, value in entry.get("vars", {}).items()
        }
        arg_names = self.current_arg_names()
        arg_index_by_original = {arg: index for index, arg in enumerate(block.args)}

        arg_pattern = "|".join(re.escape(arg) for arg in sorted(block.args, key=len, reverse=True))
        token_re = re.compile(f"{LOCAL_VAR_RE.pattern}|\\b(?:{arg_pattern})\\b") if arg_pattern else LOCAL_VAR_RE

        output = []
        cursor = 0
        out_pos = 0
        for match in token_re.finditer(block.text):
            prefix = block.text[cursor : match.start()]
            output.append(prefix)
            out_pos += len(prefix)

            token = match.group(0)
            kind = "local"
            token_id = token
            replacement = local_overrides.get(token, token)
            if token in arg_index_by_original:
                kind = "arg"
                token_id = str(arg_index_by_original[token])
                replacement = arg_names[arg_index_by_original[token]]

            start_pos = out_pos
            output.append(replacement)
            out_pos += len(replacement)
            self.token_ranges.append((start_pos, out_pos, kind, token_id, replacement))
            cursor = match.end()

        output.append(block.text[cursor:])
        self.text.setPlainText("".join(output))
        self.apply_token_formatting()

    def apply_token_formatting(self) -> None:
        self.update_extra_selections()

    def update_extra_selections(self) -> None:
        selections = []

        token_format = QtGui.QTextCharFormat()
        token_format.setForeground(QtGui.QBrush(QtGui.QColor("#075985")))
        for start, end, _kind, _token_id, _replacement in self.token_ranges:
            cursor = self.text.textCursor()
            cursor.setPosition(start)
            cursor.setPosition(end, QtGui.QTextCursor.KeepAnchor)
            selection = QtWidgets.QTextEdit.ExtraSelection()
            selection.cursor = cursor
            selection.format = token_format
            selections.append(selection)

        if self.selected_token:
            kind, token_id, _replacement = self.selected_token
            selection_format = QtGui.QTextCharFormat()
            selection_format.setBackground(QtGui.QBrush(QtGui.QColor("#fde68a")))
            for start, end, range_kind, range_token_id, _range_replacement in self.token_ranges:
                if range_kind == kind and range_token_id == token_id:
                    cursor = self.text.textCursor()
                    cursor.setPosition(start)
                    cursor.setPosition(end, QtGui.QTextCursor.KeepAnchor)
                    selection = QtWidgets.QTextEdit.ExtraSelection()
                    selection.cursor = cursor
                    selection.format = selection_format
                    selections.append(selection)

        self.text.setExtraSelections(selections)

    def token_at_position(self, position: int):
        for start, end, kind, token_id, replacement in self.token_ranges:
            if start <= position < end:
                return kind, token_id, replacement
        return None

    def on_text_click(self, point: QtCore.QPoint) -> None:
        cursor = self.text.cursorForPosition(point)
        token = self.token_at_position(cursor.position())
        if not token:
            return
        self.selected_token = token
        kind, token_id, replacement = token
        self.selected_edit.setText(f"arg[{token_id}]" if kind == "arg" else token_id)
        self.rename_edit.setText(replacement)
        self.rename_edit.setFocus(QtCore.Qt.MouseFocusReason)
        self.rename_edit.selectAll()
        self.highlight_selected_token()

    def highlight_selected_token(self) -> None:
        self.update_extra_selections()

    def apply_rename(self) -> None:
        if not self.current_block or not self.selected_token:
            QtWidgets.QMessageBox.information(self, "No Variable Selected", "Click a variable first.")
            return
        new_name = self.rename_edit.text().strip()
        if not new_name:
            QtWidgets.QMessageBox.information(self, "Missing Name", "Enter a replacement name.")
            return

        kind, token_id, _replacement = self.selected_token
        entry = self.entry_for_current_block()
        if kind == "arg":
            index = int(token_id)
            args = list(entry.get("args", []))
            while len(args) < len(self.current_block.args):
                args.append(self.current_block.args[len(args)])
            args[index] = new_name
            entry["args"] = args
        else:
            set_local_override(entry, token_id, new_name)

        self.dirty_vars = True
        self.render_current_block()
        self.set_status(f"Renamed {self.selected_edit.text()} to {new_name} in {self.current_block.key}")

    def apply_docstring(self) -> None:
        block = self.current_block
        if not block or block.kind not in ("function", "method"):
            QtWidgets.QMessageBox.information(
                self, "No Function/Method Selected", "Select a function or method first."
            )
            return
        docstring = self.doc_text.toPlainText().strip()
        entry = self.method_docs.get(block.key)
        if isinstance(entry, list) and len(entry) >= 2:
            entry[1] = docstring
        else:
            self.method_docs[block.key] = [block.display_name, docstring]
        self.dirty_docs = True
        self.set_status(f"Updated docstring for {block.key}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("disasm", nargs="?", help="input *_disasm.gc file")
    parser.add_argument("--var-names", default=str(DEFAULT_VAR_NAMES), help="var_names JSON/JSONC file")
    parser.add_argument("--method-docs", default=str(DEFAULT_METHOD_DOCS), help="method docstrings JSON/JSONC file")
    args = parser.parse_args()

    app = QtWidgets.QApplication(sys.argv)
    window = VarNameEditor(args)
    window.show()
    sys.exit(app.exec_())


if __name__ == "__main__":
    main()
