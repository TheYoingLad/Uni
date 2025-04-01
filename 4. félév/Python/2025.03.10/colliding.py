import pygame
import random

#pygame setup
pygame.init()
screen = pygame.display.set_mode((1280, 720))
clock = pygame.time.Clock()
running = True

# Variables
dt = 0
player_pos = pygame.Vector2(screen.get_width() / 2, screen.get_height() / 2)
rect_pos = pygame.Rect(600, 300, 100, 100)
rect_color = "blue"
font = pygame.font.Font(None, 74)
collision_message = None

def draw_new_rect(screen, colour = "red", rect_pos : pygame.Rect = None) -> pygame.Rect:
    if rect_pos is None:
        rect_pos = pygame.Rect(random.randint(0, screen.get_height()-100), random.randint(0,screen.get_width()-100), 100, 100)
    pygame.draw.rect(screen, colour, rect_pos)
    return rect_pos

while running:
    # poll for events
    # pygame.QUIT event means the user clicked X to close your window
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # fill the screen with a color to wipe away anything from last frame
    screen.fill("purple")

    pygame.draw.circle(screen, "red", player_pos, 40)

    rect_pos = draw_new_rect(screen)
    
    # Check for collision
    if pygame.Rect(rect_pos).collidepoint(player_pos):
        rect_color = "purple"  # Make the rectangle disappear by blending with the background
        collision_message = font.render("Collision!", True, "white")

    # Display collision message if there is a collision
    if collision_message:
        screen.blit(collision_message, (screen.get_width() / 2 - collision_message.get_width() / 2, screen.get_height() / 2 - collision_message.get_height() / 2))

    keys = pygame.key.get_pressed()
    if keys[pygame.K_w]:
        player_pos.y -= 300 * dt
    if keys[pygame.K_s]:
        player_pos.y += 300 * dt
    if keys[pygame.K_a]:
        player_pos.x -= 300 * dt
    if keys[pygame.K_d]:
        player_pos.x += 300 * dt

    # RENDER YOUR GAME HERE

    # flip() the display to put your work on screen
    pygame.display.flip()

    dt = clock.tick(60) /1000  # limits FPS to 60

pygame.quit()