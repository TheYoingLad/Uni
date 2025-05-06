import pygame
import random

# pygame setup
pygame.init()
screen = pygame.display.set_mode((1280, 720))
clock = pygame.time.Clock()
running = True

# Variables
dt = 0
player_pos = pygame.Vector2(screen.get_width() / 2, screen.get_height() / 2)
rect_pos = pygame.Rect(600, 300, 100, 100)
rect_color = "blue"

def draw_new_rect(screen, colour="red") -> pygame.Rect:
    rect_pos = pygame.Rect(random.randint(0, screen.get_width() - 100), random.randint(0, screen.get_height() - 100), 100, 100)
    pygame.draw.rect(screen, colour, rect_pos)
    return rect_pos

while running:
    # poll for events
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # fill the screen with a color to wipe away anything from last frame
    screen.fill("purple")

    # Draw the player circle
    pygame.draw.circle(screen, "red", player_pos, 40)

    # Draw the rectangle
    pygame.draw.rect(screen, rect_color, rect_pos)

    # Check for collision
    if rect_pos.collidepoint(player_pos):
        rect_pos = draw_new_rect(screen, colour=rect_color)

    # Handle player movement
    keys = pygame.key.get_pressed()
    if keys[pygame.K_w]:
        player_pos.y -= 300 * dt
    if keys[pygame.K_s]:
        player_pos.y += 300 * dt
    if keys[pygame.K_a]:
        player_pos.x -= 300 * dt
    if keys[pygame.K_d]:
        player_pos.x += 300 * dt

    # flip() the display to put your work on screen
    pygame.display.flip()

    dt = clock.tick(60) / 1000  # limits FPS to 60

pygame.quit()
