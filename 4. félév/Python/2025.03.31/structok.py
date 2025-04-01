import struct

with open("2025.03.31/bindata.bin", "wb") as f: #bináris írás
    packer = struct.Struct("10s10s") #2db 10 hosszú encoded stringet vár
    packed_data = packer.pack("almafa".encode(), " körtefaaaaa".encode())
    f.write(packed_data)

with open("2025.03.31/bindata.bin", "rb") as f: #bináris olvasás
    data = f.readline()
    unpacker = struct.Struct("10s 10s")
    unpacked_data = unpacker.unpack(data)
    unpacked_data = [i.decode().strip("\x00") for i in unpacked_data]
    print(unpacked_data)

'''
r - read
w - write
a - append
x - exclusive creation
b - binary
t - text //default
+ - update

FileNotFoundError
FileExistsError
'''