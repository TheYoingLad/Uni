import pygame
import sys

pygame.init()

# Initial window size
width, height = 640, 480
screen = pygame.display.set_mode((width, height))
pygame.display.set_caption("Pygame Resize Example")

# Colors
WHITE = (255, 255, 255)
GRAY = (200, 200, 200)
BLACK = (0, 0, 0)

# Button class
class Button:
    def __init__(self, text, pos, size, action):
        self.rect = pygame.Rect(pos, size)
        self.text = text
        self.action = action

    def draw(self, screen):
        pygame.draw.rect(screen, GRAY, self.rect)
        font = pygame.font.SysFont(None, 24)
        text_surface = font.render(self.text, True, BLACK)
        text_rect = text_surface.get_rect(center=self.rect.center)
        screen.blit(text_surface, text_rect)

    def is_clicked(self, pos):
        return self.rect.collidepoint(pos)

def resize_window(new_size):
    global screen
    screen = pygame.display.set_mode(new_size)

# Buttons for resizing
buttons = [
    Button("Small (640x480)", (50, 50), (150, 40), lambda: resize_window((640, 480))),
    Button("Medium (800x600)", (50, 110), (150, 40), lambda: resize_window((800, 600))),
    Button("Large (1024x768)", (50, 170), (150, 40), lambda: resize_window((1024, 768)))
]

# Game loop
running = True
while running:
    screen.fill(WHITE)

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.MOUSEBUTTONDOWN:
            for button in buttons:
                if button.is_clicked(event.pos):
                    button.action()

    # Draw buttons
    for button in buttons:
        button.draw(screen)

    pygame.display.flip()

pygame.quit()
sys.exit()
