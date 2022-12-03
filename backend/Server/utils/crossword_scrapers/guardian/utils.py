import re

gridStart = "<g class=\"cells\">"
cluesStart = '<ol class="crossword__clues-list">'


def starts_with_g(text):
    try:
        return text[:3] == '<g>'
    except IndexError:
        return False


def starts_with_rect(text):
    try:
        return bool(re.search(r'<rect\s*x', text))
    except IndexError:
        return False


def get_coords_from_text(text):
    s = "<rectx=\"1\"y=\"1\"width=\"31\"height=\"31\"/>"
    [col, row, _, _] = [int(x) for x in re.findall(r"\d+", text)]

    col -= 1
    col /= 32

    row -= 1
    row /= 32

    return int(row), int(col)


def split_on_li(text):
    ind = re.search("</li>", text).end()
    return text[:ind].strip(), text[ind:].strip()


def split_on_g(text):
    ind = re.search(r'</g>', text).end()
    return text[:ind], text[ind:]


def get_clue_number(text):
    start = re.search(r'<div\s*class\s*=\s*"crossword(_)+clue(_)+number"\s*>', text).end()
    end_search = re.search(r'</div>', text)
    end = end_search.start()
    rest = end_search.end()
    clue_num = int(text[start: end])
    return clue_num, text[rest:].strip()


def get_clue_body(text):
    start = re.search(r'<div\s*class\s*=\s*"crossword(_)+clue(_)+text"\s*>', text).end()
    end = re.search(r'</div>', text).start()
    full_clue = text[start:end]

    clue_end = re.search(r'\(\s*\d+\s*([,-]\s*\d+)*\)', full_clue)

    clue = full_clue[:clue_end.start()].strip()
    lengths = [int(x) for x in re.split(r'[^\d]+', clue_end.group()[1:-1])]

    return clean((clue, lengths))


def parse_li(text):
    try:
        clueNum, text = get_clue_number(text)
        clueText, clueLen = get_clue_body(text)
        return clueNum, (clueText, clueLen)
    except IndexError:
        print("ERROR -- Index error while parsing an li element")


def get_coord_info(text):
    pos = re.search(r'<rect\s*x[^>]*>', text).group()
    num = re.search(r'>\d+<', text).group()
    return pos.strip(), num.strip()


def get_clue_number_from_text(num):
    i = int(num[1: -1])
    return i


def parse_G(text):
    try:
        (pos, num) = get_coord_info(text)
        (row, col) = get_coords_from_text(pos)
        clue_num = get_clue_number_from_text(num)

        # print(f"Clue {clue_num} is at row: {row} and column: {col}")
        return clue_num, (row, col)
    except IndexError:
        print("ERROR -- Index error while parsing a g")
        return


def clean(input):
    (clue_text, length) = input
    return re.sub(r'<[^>]*>', '', clue_text).strip(), length


def get_clues_from_html(text):
    ind = re.search(cluesStart, text).end()
    text = text[ind:].strip()
    across = {}
    while text[:3] == "<li":
        (li, text) = split_on_li(text)
        (num, clue_text) = parse_li(li)
        across[num] = clue_text
        text = text.strip()

    ind = re.search(cluesStart, text).end()
    text = text[ind:].strip()
    down = {}
    while text[:3] == "<li":
        (li, text) = split_on_li(text)
        (num, clue_text) = parse_li(li)
        down[num] = clean(clue_text)
        text = text.strip()

    return across, down


def get_grid_from_html(text):
    ind = re.search(gridStart, text).end()
    text = text[ind:].strip()
    ind = re.search(r'</svg>', text).start()
    text = text[:ind].strip()
    clueLocs = {}
    while text != "</g>":
        if starts_with_g(text):
            (g, text) = split_on_g(text)
            (num, pos) = parse_G(g)
            clueLocs[num] = pos
        elif starts_with_rect(text):
            ind = re.search(r'<rect\s*x[^>]*>', text).end()
            text = text[ind:]
        else:
            print("ERROR OCCURRED - Found string that didn't start with rect or g tag")
            print(text)
            exit()
    return clueLocs

