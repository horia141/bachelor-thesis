#!/usr/bin/env python

import Image
import getopt
import sys

convert = {"0":"00",
           "1":"01",
           "00":"00",
           "01":"01",
           "10":"10",
           "11":"11"}

usage = """image2mem [OPTION..] [IMAGEFILE]
  -o File   --output=File        Output File
  -o String --memoryname=String  Memory Name
  -t String --memorytype=String  Memory Type"""

try:
    options,remainder = getopt.gnu_getopt(sys.argv[1:],'o:n:t:',
                                          ['output=','memoryname=','memorytype='])
except getopt.GetoptError,err:
    print str(err)
    print usage
    sys.exit(1)

memOutputPath = "out.mem"
memMemoryName = "OutRam"
memMemoryType = "RAM"

for opt,arg in options:
    if opt in ('-o','--output'):
        memOutputPath = arg
    elif opt in ('-n','--memoryname'):
        memMemoryName = arg
    elif opt in ('-t','--memorytype'):
        memMemoryType = arg
    else:
        print 'Unknown option \"' + opt + '\" which baffled even getopt!'
        print usage
        sys.exit(2)

if len(remainder) == 1:
    imgi = Image.open(remainder[0])
    imgo = Image.new("RGB",imgi.size)
    imgoData= []
    memOutputFile = open(memOutputPath,"w")
    
    memOutputFile.write("Name: " + memMemoryName + "\n")
    memOutputFile.write("Type: " + memMemoryType + "\n")
    memOutputFile.write("AddrSize: " + str(imgi.size[0] * imgi.size[1]) + "\n")
    memOutputFile.write("WordSize: 6\n")
    memOutputFile.write("Format: Bin\n")
    memOutputFile.write("\n")

    for pixel in imgi.getdata():
        r = pixel[0] >> 6
        g = pixel[1] >> 6
        b = pixel[2] >> 6

        imgoData.append((85*r,85*g,85*b))
        memOutputFile.write(convert[bin(r)[2:]] + convert[bin(g)[2:]] + convert[bin(b)[2:]] + "\n")

    imgo.putdata(imgoData)
    imgo.save(memMemoryName + ".png")
    memOutputFile.close()
else:
    print 'Invalid call to image2mem!'
    print usage
    sys.exit(3)
