#!/usr/bin/python

import random

letters = map(chr, range(ord('a'), ord('z') + 1)) + map(str, range(0, 10))

key_letters = []

while len(key_letters) < 64:
    key_letters.append(random.choice(letters))

key = ''.join(key_letters)

print key




