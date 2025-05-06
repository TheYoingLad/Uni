import pygame

#pygame setup
pygame.init()
pygame.display.set_caption('Base exercise')

screen = pygame.display.set_mode((1280, 720))
clock = pygame.time.Clock()
running = True

global display_FPS
display_FPS = False

def show_FPS(color, font, size):
  
    # creating font object score_font 
    score_font = pygame.font.SysFont(font, size)
    
    # create the display surface object
    # score_surface
    score_surface = score_font.render('FPS : ' + str(clock.get_fps()), True, color)
    
    # create a rectangular object for the 
    # text surface object
    score_rect = score_surface.get_rect()
    
    # displaying text
    screen.blit(score_surface, score_rect)

while running:
    # poll for events
    # pygame.QUIT event means the user clicked X to close your window
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # fill the screen with a color to wipe away anything from last frame
    screen.fill("purple")

    keys = pygame.key.get_pressed()
    if keys[pygame.K_w]:
        pygame.draw.circle(screen, "red", pygame.Vector2(screen.get_width()/2, screen.get_height()/2), 40)
    if keys[pygame.K_s]:
        pygame.draw.circle(screen, "blue", pygame.Vector2(screen.get_width()/2, screen.get_height()/2), 40)
    if keys[pygame.K_a]:
        pygame.draw.circle(screen, "black", pygame.Vector2(screen.get_width()/2, screen.get_height()/2), 40)
    if keys[pygame.K_d]:
        pygame.draw.circle(screen, "pink", pygame.Vector2(screen.get_width()/2, screen.get_height()/2), 40)
    if keys[pygame.K_q]:
        current_time = pygame.time.get_ticks()
        if not hasattr(show_FPS, 'last_toggle') or current_time - show_FPS.last_toggle > 100:
            show_FPS.last_toggle = current_time
            display_FPS = not display_FPS
    if keys[pygame.K_ESCAPE]:
        running = False
    
    if display_FPS:
        show_FPS('white', 'comicsansms', 32)

    # flip() the display to put your work on screen
    #pygame.display.update(pygame.draw.circle(screen, "pink", pygame.Vector2(screen.get_width()/2, screen.get_height()/2), 40))
    pygame.display.flip() # == pygame.display.update() without any arguments
    clock.tick(60)  # limits FPS to 60
    

pygame.quit()