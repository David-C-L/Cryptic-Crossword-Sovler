import cv2 as cv2
import pytesseract

# from crossword_puzzle.crossword_puzzle import CrosswordPuzzle
from .crossword_puzzle.crossword_puzzle import CrosswordPuzzle

import re


class CrosswordImageProcessor:

    @staticmethod
    def crossword_from_images(tesseract_path, grid_img, across_clues_img, down_clues_img, rows: int, cols: int):
        """
        Function that takes in a picture of a grid, across and down clues,
        and verifying that the clues match the grid
        :param tesseract_path: path to the Tesseract executable
        :param grid_img: image of the grid from cv2.imread()
        :param across_clues_img: image of the across clues from cv2.imread()
        :param down_clues_img: image of the down clues from cv2.imread()
        :param rows: number of rows in the grid
        :param cols: number of columns in the grid
        """

        crossword_puzzle = CrosswordPuzzle()

        print("Uploading grid...")
        CrosswordImageProcessor.__grid_from_image(
            crossword_puzzle=crossword_puzzle,
            img=grid_img,
            rows=rows,
            cols=cols
        )

        print("Uploading across clues...")
        CrosswordImageProcessor.__clues_from_image(
            tesseract_path=tesseract_path,
            crossword_puzzle=crossword_puzzle,
            img=across_clues_img,
            is_across=True
        )

        print("Uploading down clues...")
        CrosswordImageProcessor.__clues_from_image(
            tesseract_path=tesseract_path,
            crossword_puzzle=crossword_puzzle,
            img=down_clues_img,
            is_across=False
        )

        print("Verifying puzzle state...")
        crossword_puzzle.verify_and_sync()

        return crossword_puzzle

    @staticmethod
    def __grid_from_image(crossword_puzzle: CrosswordPuzzle, img, rows: int, cols: int):
        """
        Take an image with a crossword grid and store it in the class
        :param crossword_puzzle: the crossword puzzle being modified
        :param img: image object from cv2.imread()
        :param rows: number of rows in the grid
        :param cols: number of columns in the grid
        """

        # Set the grid dimensions
        crossword_puzzle.set_grid(rows, cols)

        # Convert the image to grayscale
        gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

        # Thresholding
        ret, thresh = cv2.threshold(gray, 127, 255, cv2.THRESH_BINARY_INV)
        thresh2 = cv2.bitwise_not(thresh)

        # Find contours in the image
        contours, hierarchy = cv2.findContours(thresh, cv2.RETR_EXTERNAL, 1)

        max_area = -1
        max_cnt = -1

        # Locate the grid by finding the square with the largest area
        for cnt in contours:
            # Get the approximated contour
            approx = cv2.approxPolyDP(cnt, 0.02 * cv2.arcLength(cnt, True), True)
            if len(approx) == 4 and cv2.contourArea(cnt) > max_area:
                # Found largest rectangle, store this information
                max_area = cv2.contourArea(cnt)
                max_cnt = cnt

        # Extract the crossword region, and resize it to a standard size
        x, y, w, h = cv2.boundingRect(max_cnt)
        cross_rect = thresh2[y:y + h, x:x + w]
        cross_rect = cv2.resize(cross_rect, (cols * 10, rows * 10))

        # Iterate through each cell, treating it as empty if more than 50 pixels are white
        for i in range(rows):
            for j in range(cols):
                box = cross_rect[i * 10:(i + 1) * 10, j * 10:(j + 1) * 10]
                if cv2.countNonZero(box) > 50:
                    crossword_puzzle.turn_cell_white(i, j)

    @staticmethod
    def __clues_from_image(tesseract_path, crossword_puzzle: CrosswordPuzzle, img, is_across: bool):
        """
        Uploads a column of clues to the data structure using regexes and string manipulation
        :param tesseract_path: path to the Tesseract executable
        :param crossword_puzzle: the crossword puzzle being modified
        :param img: image object from cv2.imread()
        :param is_across: True if the clues are from the across column, False otherwise
        """

        pytesseract.pytesseract.tesseract_cmd = tesseract_path

        # TODO: IMAGE PREPROCESSING

        # Convert the image to a string
        text = pytesseract.image_to_string(img, lang='eng', config=f'--psm 6')

        # TEXT POST PROCESSING

        # Remove Across/Down if needed
        text = re.sub('^across' if is_across else '^down', '', text, flags=re.IGNORECASE)

        # Change vertical bars to "I"
        text = text.replace('|', 'I')

        # Replace newlines with spaces
        text = text.replace('\n', ' ')

        # EXTRACT THE CLUES

        # Split text based on the answer length found at the end of a clue
        split_text = re.split(r'(\([1-9][0-9]?(, ?[1-9][0-9]?)*\))', text)

        # The above regex uses two capture groups, and .split() returns both
        # in the result (returning None if the 2nd capture group isn't invoked)
        del split_text[2::3]

        # Group each clue with its answer length (the element succeeding the clue in the list
        split_text_iter = iter(split_text)
        clue_length_tuples = list(zip(split_text_iter, split_text_iter))

        # Process each clue
        for clue, length in clue_length_tuples:

            # Search for the 2 digit number in the clue after removing extra whitespace
            clue_no_search = re.match(r'([1-9][0-9]?)\.?(.*)', clue.lstrip().rstrip())

            # Search for the actual value of the length
            length_val_search = re.match(r'\((.*)\)', length)

            # Check that matches were successful
            if not clue_no_search:

                raise ValueError(f"Couldn't detect the clue number from the image for {clue}")

            elif not length_val_search:

                raise ValueError(f"Couldn't detect the answer length from the image for {clue}")

            else:

                # Convert to relevant types and remove whitespace at start/end
                clue_no = int(clue_no_search.group(1))
                clue_text = clue_no_search.group(2).lstrip().rstrip()

                length_val_match = length_val_search.group(1)

                # Remove whitespace
                length_val_match = length_val_match.replace(" ", "")

                if ',' in length_val_match:
                    word_lengths = length_val_match.split(',')
                    answer_len = list(map(int, word_lengths))
                else:
                    answer_len = [int(length_val_match)]

                # Store the clue in the respective grid's map
                crossword_puzzle.add_clue(clue_no, is_across, clue_text, answer_len)


if __name__ == "__main__":

    grid_img = cv2.imread("test_images/6_grid.jpg")
    across_img = cv2.imread("test_images/6_clues_across.jpg")
    down_img = cv2.imread("test_images/6_clues_down.jpg")
    image_puzzle = CrosswordImageProcessor.crossword_from_images(
        tesseract_path=r"C:\\Program Files\\Tesseract-OCR\\tesseract.exe",
        grid_img=grid_img,
        across_clues_img=across_img,
        down_clues_img=down_img,
        rows=15,
        cols=15
    )

    print(image_puzzle)
