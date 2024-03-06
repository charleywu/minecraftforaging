import numpy as np
import cv2
import os

### Program constants
player_data_path = "data\\players.log.csv"
block_data_path = "data\\blocks.log.csv"

image_folder = "tmp"
video_name = "video.avi"
field_image = "res\\field_bg.png"
player_image = "res\\Steve.png"
tmp_field_filename = "tmp_field.png"

brown_color = (0,5,20)
blue_color = (230, 130, 50)
view_vector_color = (0,255,0)

rectangle_size = 8
coordiante_interval = 62 # 20*3 + 2 (Padding)
fps = 20
player_amount = 4
view_vector_length = 20
rows_to_read = 10000000


"""
Inserts a subimage into a main image at an offset. (Insert Minecraft-Steve head's at the player positions)
Args:   main: Main image to draw on top of
        sub: Subimage to draw on top of main
        x_offset: X-Coordiante offset of sub image on main image
        y_offset: Y-Coordiante offset of sub image on main image
        center: If set to true, the sub image will be centered at (x_offset, y_offset)
"""
def insert_subimg(main, sub, x_offset, y_offset, center=False):
    if center:
        x_offset -= int(sub.shape[0]*0.5)
        y_offset -= int(sub.shape[1]*0.5)
    x_offset = max(0, x_offset)
    y_offset = max(0, y_offset)
    main[y_offset:y_offset+sub.shape[0], x_offset:x_offset+sub.shape[1]] = sub

"""
Reads all files 1.png, 2.png, 3.png .... [frames].png from the /tmp folder and compiles them into a video
Args:   frames: Number of images to include in video
"""
def make_video(frames):
    # String together tmp/1.png ... tmp/[frames].png
    images = [str(u)+".png" for u in range(0,frames)]
    frame = cv2.imread(os.path.join(image_folder, images[0]))
    height, width, layers = frame.shape

    fourcc = cv2.VideoWriter_fourcc(*'mp4v') # Use mp4v compression. If there is a problem uncomment the line below to disable compression (Note: will lead to videos of some 800Mb)
    # fourcc = 0

    video = cv2.VideoWriter(video_name, fourcc, fps, (width,height))

    for image in images:
        video.write(cv2.imread(os.path.join(image_folder, image)))

    cv2.destroyAllWindows()
    video.release()

"""
After a block (melon/pumpkin) has been broken, fill the respective square on the map with either a brown or blue square, depeding on reward
Args:   reward_found: If true, color block in blue else in brown
        img: Image to draw our blue or brown squares on. In this case it's res/field_bg.png
        x: x coordinate of broken block / Where to color in our brown/blue square
        y: y coordinate of broken block / Where to color in our brown/blue square
"""
def set_field(reward_found, img, x, y):
    x = int(x / coordiante_interval * img.shape[0])
    y = int(y / coordiante_interval * img.shape[1])
    if not reward_found:
        # Draw brown rectange on top of green one
        cv2.rectangle(img, (x, y), (x+rectangle_size, y+rectangle_size), brown_color, -1)
    else:
        # Draw blue rectange on top of green one
        cv2.rectangle(img, (x, y), (x+rectangle_size, y+rectangle_size), blue_color, -1)
    # Save the changes to the field
    cv2.imwrite(image_folder+"\\"+tmp_field_filename, img)


## ======================================================================
# Here we read the data from data/blocks.log.csv and data/players.log.csv

# Conversion functions for parsing:
f2f = lambda x: float(x)
b2b = lambda x: str(x) == "b'true'"

# Store player-data into table and block data into btable
table = np.genfromtxt(player_data_path, delimiter=';', skip_header=1, max_rows=rows_to_read, names=['time', 'name', 'x', 'z', 'xlook', 'ylook', 'zlook'], converters = {'time': f2f, 'x':f2f, 'z':f2f, 'xlook':f2f, 'ylook':f2f, 'zlook':f2f}, dtype='object')
btable = np.genfromtxt(block_data_path, delimiter=';', skip_header=1, max_rows=rows_to_read, names=['time', 'name', 'x', 'z', 'reward'], converters = {'time': f2f, 'x':f2f, 'z':f2f, 'reward':b2b}, dtype='object')

# General program flow:
# We store the current "background" (current block-states) into a tmp_field.png file that we update each time a block gets broken
# While doing this, for each recorded frame (every 4 entries) in 'table', we draw current player positions and view directions on top
#   and save it as a x.png file into the tmp/ directory. In a final step we turn all of these files into a video.

bi = 0 # bi takes into account the current index of our block-data (btable)
# Copy the original background (res/field_bg.png) where all blocks are green into tmp/tmp_field.png so we can draw on top of it
#   without modifying the original
cv2.imwrite(image_folder+"\\"+tmp_field_filename, cv2.imread(field_image))

# For every 4 entries in table, do the following:
for i in range(0, len(table)-player_amount, player_amount):
    # Output precentage number in steps of 10% completion
    if(100*i/len(table) % 10 == 0): print("\r"+str(100*i/len(table))+"% complete\r", end='')

    # Retrieve the most recent field image
    field_img = cv2.imread(image_folder+"\\"+tmp_field_filename)
    # Read the image we use to portray players (Steve head)
    player_img  = cv2.imread(player_image)

    # If we are further ahead in time in table than in btable, catch up and draw the block-changes
    while bi<len(btable) and table['time'][i] >= btable['time'][bi]:
        set_field(btable['reward'][bi], field_img, int(btable['x'][bi]), int(btable['z'][bi]))
        bi += 1

    # Draw each of the 4 players
    for j in range(player_amount):
        # Player coordinates
        x = int(table['x'][i+j] / coordiante_interval * field_img.shape[0])
        z = int(table['z'][i+j] / coordiante_interval * field_img.shape[1])

        # Player front-vector
        xlook = table['xlook'][i+j]
        ylook = table['ylook'][i+j]
        zlook = table['zlook'][i+j]

        # Draw view direction
        ylook = 0.00001 if ylook==0 else ylook #Cant divide by 0 ---> 0.00001
        v_vector = (x+int(view_vector_length*xlook), z+int(view_vector_length*zlook))
        cv2.line(field_img, (x, z), v_vector, view_vector_color, 2) # line thcikness of 2

        # Draw player symbol
        insert_subimg(field_img, player_img, x, z, True)

    # Draw most recent frame
    cv2.imwrite(image_folder+"\\"+str(int(i/player_amount))+".png", field_img)

# All frames have been drawn. Make a video
make_video(len(table))
