from PIL import Image
import sys
import random

im = Image.open('test-2.png')
pixelMap = im.load()
sys.setrecursionlimit(30000)
sz = im.size


def flood_fill(img, x, y):
    # print(img[x, y])
    if x < 0 or x >= sz[1] or y < 0 or y >= sz[1] or img[x, y] == (255, 255, 255):
        return img
    img[x, y] = (255, 255, 255, 255)
    img = flood_fill(img, x + 1, y)
    img = flood_fill(img, x, y + 1)
    img = flood_fill(img, x - 1, y)
    img = flood_fill(img, x, y - 1)
    return img


im2 = Image.new("RGBA", sz)
pm2 = im2.load()

for i in range(im2.size[0]):
    for j in range(im2.size[1]):
        pm2[i, j] = (255, 255, 255)

cnt = 1
classes = {}
colors = {}

dset = 'df = data.frame(x = double(), y = double(), class = character())\n\n'
points = 'points = array(c('
cntGrey = 0

try:
    for i in range(im.size[0]):
        for j in range(im.size[1]):
            if pixelMap[i, j] != (255, 255, 255):
                if pixelMap[i + 5, j + 5] != (106, 106, 106) and pixelMap[i + 5, j + 5] not in classes:
                    classes[pixelMap[i + 5, j + 5]] = "class-" + str(cnt)
                    colors['class-' + str(cnt)] = (random.randint(0, 255), random.randint(0, 255), random.randint(0, 255), 255)
                    cnt += 1

                if pixelMap[i + 5, j + 5] != (106, 106, 106):
                    dset += 'df <- rbind(df, data.frame(x = {}, y = {}, class = "{}"))\n'.format(i, j, classes[pixelMap[i + 5, j + 5]])
                else:
                    points += 'c({}, {})'.format(i, j)
                    cntGrey += 1

                for k in range(i, i + 25):
                    for m in range(j, j + 25):
                        pm2[k, m] = colors[classes[pixelMap[i + 5, j + 5]]] if pixelMap[i + 5, j + 5] in classes else (0, 0, 0)

                # print(i, j, pixelMap[i, j], classes[pixelMap[i, j]])

                pixelMap = flood_fill(pixelMap, i, j)
    points += '), dim = c({},2))'.format(cntGrey)
    print(dset)
    if len(points) > 0:
        print(points)
except RecursionError:
    print('Something went wrong.')

# im.save("test2.png")
im2.save("test3.png") # demo of recognized points
