#!usr/bin/python # coding: utf-8
import sys
from PIL import Image, ImageDraw
from random import randint
from pickle import load

b = 2
h = 50

lCodes = [[0,0,0,1,1,0,1],
          [0,0,1,1,0,0,1],
          [0,0,1,0,0,1,1],
          [0,1,1,1,1,0,1],
          [0,1,0,0,0,1,1],
          [0,1,1,0,0,0,1],
          [0,1,0,1,1,1,1],
          [0,1,1,1,0,1,1],
          [0,1,1,0,1,1,1],
          [0,0,0,1,0,1,1]]

rCodes = [[1,1,1,0,0,1,0],
          [1,1,0,0,1,1,0],
          [1,1,0,1,1,0,0],
          [1,0,0,0,0,1,0],
          [1,0,1,1,1,0,0],
          [1,0,0,1,1,1,0],
          [1,0,1,0,0,0,0],
          [1,0,0,0,1,0,0],
          [1,0,0,1,0,0,0],
          [1,1,1,0,1,0,0]]

def generate(code):
    code = '0' * (8 - len(code)) + code

    barcode = [0 for i in range(67)]

    barcode[0], barcode[1], barcode[2] = 1, 0, 1
    
    for i in range(28):
        barcode[i + 3] = lCodes[int(code[i / 7])][i % 7]
    
    barcode[31], barcode[32], barcode[33], barcode[34], barcode[35] = 0, 1, 0, 1, 0
    
    for i in range(28):
        barcode[i + 36] = rCodes[int(code[i / 7 + 4])][i % 7]
    
    barcode[64], barcode[65], barcode[66] = 1, 0, 1

    draw = ImageDraw.Draw(image)
    for i in range(67):
        draw.rectangle([(i * b, 0), ((i + 1) * b, h)], fill = ['white', 'black'][barcode[i]])
    
    image.save(code + '.png')

if len(sys.argv) != 2:
    print 'Falsche Anzahl an Argumenten'
    print(len(sys.argv))
    sys.exit

image = Image.new('RGB', (134, 50), 'black')

try:
    generate(sys.argv[1])
except:
    print('Generieren von Barcode für', sys.argv[1], 'fehlgeschlagen')

