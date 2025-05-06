# Importok
import pygame
import time
import random

# Előre definiált színek
black = pygame.Color(0, 0, 0)
white = pygame.Color(255, 255, 255)
red = pygame.Color(255, 0, 0)
green = pygame.Color(0, 255, 0)
blue = pygame.Color(0, 0, 255)

#----------Feladat-----------------------
# Hozd létre a kígyó sebességét kontroláló változót, ez lesz az FPS
speed = 0

# Hozd létre az ablak méretét tároló változókat
width, height = 1280, 720

#Inicializáld a pygame-t
pygame.init()

# Inicializád az ablakot (Caption -> opcionális, size -> kötelező)
screen = pygame.display.set_mode((width, height))

# Hozd létre a játékórát, ez lesz a játék sebességét szabályzó változó
clock = pygame.time.Clock()

# Hozz létre egy két elemű listát, amely a kígyó pozícióját tárolja
snake_pos = pygame.Vector2(screen.get_width()/2, screen.get_height()/2)

# Hozz létre egy listák listáját, ami a kígyó testét tárolja
snake_pos = []

# Hozd létre a gyümölcs pozícióját tároló listát
apple_pos = pygame.Rect(random.randint(0, screen.get_width()-50), random.randint(0, screen.get_height()-50), 50, 50)

# Hozz létre egy változót, ami azt tárolja, hogy a gyümölcs megjelent-e
display_apple = False

# Hozz létre egy változót, ami tárolja a kígyó irányát
snake_dir = [-1, 0]

# implementáld a függvényt ami azért felel, hogy leellenőrizze, a játék véget ért-e.
# Amennyiben igen, akkor áljon le a játék
def game_over():
    return NotImplementedError


def draw_snake():
    pass

def move_snake():
    pass

# Fő ciklus
running = True
while running:

    # Fejezd be a for-ciklust, ami kezeli a leütött billentyűket
    for event in pygame.event.get():
        if event.type == pygame.KEYDOWN:
            keys = pygame.key.get_pressed()
            if keys[pygame.K_ESCAPE]:
                running = False

    # Kezeld le azokat az eseteket, amikor a kígyónak magába kéne harapnia
    # pl: ha jobbra megy és megnyomod a ball gombot vagy az "a"-t
    keys = pygame.key.get_pressed()
    if keys[pygame.K_w] and snake_dir != 2:
        snake_dir = 0
    if keys[pygame.K_d] and snake_dir != 3:
        snake_dir = 1
    if keys[pygame.K_s] and snake_dir != 0:
        snake_dir = 2
    if keys[pygame.K_a] and snake_dir != 1:
        snake_dir = 3

    # Hozd létre a kígyó mozgásáért felelős mechanizmust. 
    # Frissítsd a kígyó pozícióját a megfelelő irányba
    current_time = pygame.time.get_ticks()
    if not hasattr(draw_snake, 'last_toggle') or current_time - draw_snake.last_toggle > 100:
        draw_snake.last_toggle = current_time

    # Kezeld le, hogy amennyiben a kígyó feje és a gyümölcs átfedik egymást 
    # akkor a kígyó testéhez adj hozzá egy újabb elemet

    # Amikor a kígyó megette a gyümölcsöt, akkor generálj egy új gyümölcsöt
    
    # Rajzold ki az összes elemet: kígyó feje kígyó teste és a gyümölcs

    # A játék végét itt kezeld le
    if game_over():
        running = False
    
    # Frissítsd a képernyőt
    pygame.display.flip()

    # Hívd meg a játékórát a játék sebességének szabályozására létrehozott változóval
    speed = clock.tick(60) / 1000

pygame.quit()