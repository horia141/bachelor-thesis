#!/usr/bin/env python

import Image
import cStringIO
import getopt
import sys

def convert(size,number):
    assert size > 0

    repr = bin(number)[2:]
    return ((size-len(repr)) * '0') + repr

memTypes = ['RAM','ROM']
channelsTypes = ['L','RGB','LRGB']
maxChannelBits = 8

usage = """image2mem [OPTION..] [IMAGEFILE]
  -o File   --output=File          Output File
  -o String --memoryname=String    Memory Name
  -t String --memorytype=String    Memory Type""" +  str(memTypes) + """
  -c String --channelstype=String  Channel Type""" + str(channelsTypes) + """
  -b Number --channelbits=Number   Number of Bits per Channel"""

try:
    options,remainder = getopt.gnu_getopt(sys.argv[1:],'o:n:t:c:b:',
                                          ['output=','memoryname=','memorytype=','channelstype=','channelbits='])
except getopt.GetoptError,err:
    print str(err)
    print usage
    sys.exit(1)

memOutputPath   = 'out.mem'
memMemoryName   = 'OutRam'
memMemoryType   = 'RAM'
memChannelsType = 'RGB'
memChannelBits  = 2

for opt,arg in options:
    if opt in ('-o','--output'):
        memOutputPath = arg
    elif opt in ('-n','--memoryname'):
        memMemoryName = arg
    elif opt in ('-t','--memorytype'):
        memMemoryType = arg
    elif opt in ('-c','--channelstype'):
        memChannelsType = arg
    elif opt in ('-b','--channelbits'):
        try:
            memChannelBits = int(arg)
        except ValueError,err:
            print 'Parameter to channelbits argument must be positive integer!'
            print usage
            sys.exit(1)
    else:
        print 'Unknown option \"' + opt + '\" which baffled even getopt!'
        sys.exit(2)

if memMemoryType not in memTypes:
    print 'Unknown memory type \"' + memMemoryType + '\"!'
    print 'Supported types are ' + str(memTypes) + '!'
    sys.exit(1)

if memChannelsType not in channelsTypes:
    print 'Unknown channels type \"' + memChannelsType + '\"!'
    print 'Supported types are ' + str(channelsTypes) + '!'
    sys.exit(1)

if memChannelBits > maxChannelBits:
    print 'Cannot have more than ' + str(maxChannelBits) + ' bits on each channel!'
    sys.exit(1)

if memChannelBits < 1:
    print 'Cannot have less than 1 bit on each channel!'
    sys.exit(1)

if len(remainder) == 1:
    imgi = Image.open(remainder[0])
    imgo = Image.new("RGB",imgi.size)
    imgoData= []
    memOutputFile = open(memOutputPath,"w")
    memOutputBody = cStringIO.StringIO()

    interval = 255 / (2**memChannelBits - 1)

    memOutputFile.write('Name: ' + memMemoryName + '\n')
    memOutputFile.write('Type: ' + memMemoryType + '\n')
    memOutputFile.write('AddrSize: ' + str(imgi.size[0] * imgi.size[1]) + '\n')

    if memChannelsType == 'L':
        memOutputFile.write('WordSize: ' + str(memChannelBits) + '\n')
        for pixel in imgi.getdata():
            l = int(0.30 * pixel[0] + 0.59 * pixel[1] + 0.11 * pixel[2]) >> (maxChannelBits - memChannelBits)

            imgoData.append((interval*l,interval*l,interval*l))
            memOutputBody.write(convert(memChannelBits,l) + '\n')
    elif memChannelsType == 'RGB':
        memOutputFile.write('WordSize: ' + str(3 * memChannelBits) + '\n')

        for pixel in imgi.getdata():
            r = pixel[0] >> (maxChannelBits - memChannelBits)
            g = pixel[1] >> (maxChannelBits - memChannelBits)
            b = pixel[2] >> (maxChannelBits - memChannelBits)

            imgoData.append((interval*r,interval*g,interval*b))
            memOutputBody.write(convert(memChannelBits,r) + convert(memChannelBits,g) + convert(memChannelBits,b) + '\n')
    elif memChannelsType == 'LRGB':
        memOutputFile.write('WordSize: ' + str(4 * memChannelBits) + '\n')

        for pixel in imgi.getdata():
            r = pixel[0] >> (maxChannelBits - memChannelBits)
            g = pixel[1] >> (maxChannelBits - memChannelBits)
            b = pixel[2] >> (maxChannelBits - memChannelBits)
            l = int(0.30 * pixel[0] + 0.59 * pixel[1] + 0.11 * pixel[2]) >> (maxChannelBits - memChannelBits)

            imgoData.append((interval*(l + r)/2,interval*(l + g)/2,interval*(l + b)/2))
            memOutputBody.write(convert(memChannelBits,l) + convert(memChannelBits,r) + convert(memChannelBits,g) + convert(memChannelBits,b) + '\n')
    else:
        print 'We shouldn\'t have arrived here!'
        sys.exit(2)

    memOutputFile.write('Format: Bin\n')
    memOutputFile.write('Generator:\n')
    memOutputFile.write('    Name: ImageToMem\n')
    memOutputFile.write('    Data:\n')
    memOutputFile.write('        ChannelsType: ' + memChannelsType + '\n')
    memOutputFile.write('        ChannelBits: ' + str(memChannelBits) + '\n')
    memOutputFile.write('\n')
    memOutputFile.write(memOutputBody.getvalue())
    memOutputFile.write('\n')
    memOutputFile.close()

    imgo.putdata(imgoData)
    imgo.save(memMemoryName + '.png')
else:
    print 'Invalid call to image2mem!'
    print usage
    sys.exit(1)
