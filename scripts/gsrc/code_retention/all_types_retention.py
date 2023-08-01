from utils import get_alltypes_path_from_game, get_gsrc_path_from_filename


class AllTypesUpdateBlock:
    def __init__(self):
        self.data = []
        self.file_name = ""
        self.block_id = ""

    def __str__(self):
        return "{}:{}:{}...".format(self.file_name, self.block_id, self.data[0:20])


def update_alltypes_named_blocks(game_name):
    block_dict = {}
    # Step 1: Get the blocks
    get_all_blocks(game_name, block_dict)
    # Step 2: Update the blocks (group by file name to minimize file IO operations)
    update_all_blocks(game_name, block_dict)


def get_all_blocks(game_name, block_dict):
    with open(get_alltypes_path_from_game(game_name)) as f:
        lines = f.readlines()
        i = 0
        while i < len(lines):
            line = lines[i]
            if line.startswith(";; +++") and ":" in line:
                info = line.replace(";; +++", "")
                file_name, block_id = info.split(":")
                new_block = AllTypesUpdateBlock()
                new_block.file_name = file_name
                new_block.block_id = block_id
                # Loop until we find the end of the block, collecting the lines as we go
                while i < len(lines):
                    i = i + 1
                    next_line = lines[i]
                    if next_line.startswith(";; ---"):
                        break
                    new_block.data.append(next_line)
                # Add to the dictionary
                if file_name not in block_dict:
                    block_dict[file_name] = [new_block]
                else:
                    block_dict[file_name].append(new_block)
            else:
                i = i + 1


def update_all_blocks(game_name, block_dict):
    for file_name, blocks in block_dict.items():
        # Get the file's lines
        path = get_gsrc_path_from_filename(game_name, file_name)
        lines = []
        final_lines = []
        with open(path) as f:
            lines = f.readlines()
        # Iterate through lines, (before ;; decomp begins) and update the blocks if we find them
        i = 0
        while i < len(lines):
            line = lines[i]
            if line.lower().startswith(";; decomp begins"):
                final_lines.append(line)
                # Add all the rest of the lines until the end
                while i + 1 < len(lines):
                    i = i + 1
                    next_line = lines[i]
                    final_lines.append(next_line)
                break
            if line.startswith(";; +++"):
                final_lines.append(line)
                block_id = line.split(";; +++")[1]
                # Look to see if we actually have that block
                found_block = False
                for block in blocks:
                    if block.block_id == block_id:
                        found_block = True
                        # if we found the block, write the data, then proceed ahead until the end
                        for block_line in block.data:
                            final_lines.append(block_line)
                        while i + 1 < len(lines):
                            i = i + 1
                            next_line = lines[i]
                            if next_line.startswith(";; ---"):
                                final_lines.append(next_line)
                                i = i + 1
                                break
                        break
                if not found_block:
                    i = i + 1
            else:
                final_lines.append(line)
                i = i + 1
        # Update the file contents
        # Don't write a new file unless we have to, though
        if lines != final_lines:
            with open(path, "w") as f:
                f.writelines(final_lines)
