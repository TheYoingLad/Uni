using Snake.Model;
using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.Input;
using Avalonia.Threading;
using System;
using Avalonia.Media;
using System.Runtime.InteropServices;
using CommunityToolkit.Mvvm.ComponentModel;

namespace Snake.Avalonia.ViewModels
{
    public class MainViewModel : ObservableObject
    {
        #region Fields

        // GameModel instance
        private GameModel model;
        
        // Tile Property Object List => updates View if elements are added or removed
        public ObservableCollection<RectangleViewModel> Tiles { get; private set; } = new ();

        // Property variables
        private int resolution = -1;
        private bool menuBoxVisibility = true;
        private bool loadedLabelVisibility = false;
        private bool scoreLabelVisibility = false;
        private string scoreLabelText = "Score: 000";
        private string gameOverLabelText = "";
        private string startBtnText = "Play";
        private Brush gameOverLabelColor = new SolidColorBrush(new Color(255, 255, 0, 0));

        #endregion

        #region Properties

        // Const window size
        private const double windowSize = 750d;

        // Properties (ViewModel-to-View Communication mainly)
        public int Resolution
        {
            get => resolution;
            private set
            {
                resolution = value;
                OnPropertyChanged();
            }
        }

        public bool MenuBoxVisibility
        {
            get => menuBoxVisibility;
            set
            {
                menuBoxVisibility = value;
                OnPropertyChanged();
            }
        }

        public bool LoadedLabelVisibility
        {
            get => loadedLabelVisibility;
            set
            {
                loadedLabelVisibility = value;
                OnPropertyChanged();
            }
        }

        public bool ScoreLabelVisibility
        {
            get => scoreLabelVisibility;
            set
            {
                scoreLabelVisibility = value;
                OnPropertyChanged();
            }
        }

        public string ScoreLabelText
        {
            get => scoreLabelText;
            set
            {
                scoreLabelText = value;
                OnPropertyChanged();
            }
        }

        public string GameOverLabelText
        {
            get => gameOverLabelText;
            set
            {
                gameOverLabelText = value;
                OnPropertyChanged();
            }
        }

        public string StartBtnText
        {
            get => startBtnText;
            set
            {
                startBtnText = value;
                OnPropertyChanged();
            }
        }

        public Brush GameOverLabelColor
        {
            get => gameOverLabelColor;
            set
            {
                gameOverLabelColor = value;
                OnPropertyChanged();
            }
        }

        public bool IsDesktop
        {
            get
            {
                return (RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ||
                         RuntimeInformation.IsOSPlatform(OSPlatform.OSX) ||
                         RuntimeInformation.IsOSPlatform(OSPlatform.Linux));
            }
        }

        public bool ArrowBtnVisibility
        {
            get => !IsDesktop && model.Ingame;
        }

        // Delegate Commands (View-to-ViewModel Communication)
        public RelayCommand PlayCommand { get; private set; }
        public RelayCommand LoadCommand { get; private set; }
        public RelayCommand ExitCommand { get; private set; }
        public RelayCommand<string> KeyDownCommand { get; private set; }

        // App-to-ViewModel Communication
        public event EventHandler? StartGame;
        public event EventHandler? LoadMap;
        public event EventHandler? ExitGame;

        #endregion

        #region Constructor(s)

        public MainViewModel(GameModel model)
        {
            // Parameterize Model
            this.model = model;
            this.model.ResetTiles += ModelUpdateAllTiles;
            this.model.UpdateTile += ModelUpdateSingleTile;
            this.model.StopEvent += ModelStopGame;

            // Initialize DelegateCommands
            PlayCommand = new RelayCommand(OnGamestart);
            LoadCommand = new RelayCommand(OnLoadMap);
            ExitCommand = new RelayCommand(OnExitGame);
            KeyDownCommand = new RelayCommand<string>(WindowKeyDown);
        }

        public MainViewModel() : this(new GameModel()) { }

        #endregion

        #region Utilities

        /// <summary>
        /// Sets the properties of view elements accordingly when a map is loaded in 
        /// </summary>
        public void MapLoadingViewChange()
        {
            LoadedLabelVisibility = true;
            ScoreLabelVisibility = false;

            GameOverLabelText = "";
            StartBtnText = "Play";
        }

        /// <summary>
        /// Pauses the Game
        /// </summary>
        public void PauseGame()
        {
            if (!MenuBoxVisibility) model.StopGame(gameOver: false);
            else if (model.Ingame)
            {
                MenuBoxVisibility = false;
                model.StartGame(newGame: !model.Ingame);
            }
        }

        #endregion

        #region Model-to-ViewModel Event Methods

        private void ModelUpdateAllTiles(object? sender, EventArgs? e)
        {
            // Reconstruct PictureBox Grid if needed (resolution has changed)
            if (this.Resolution != model.Resolution)
            {
                this.Resolution = model.Resolution;

                // Delete previous Tiles from Form Control and from List
                Tiles.Clear();

                // Generating Tiles to cover canvas
                double tileSize = windowSize / (float)resolution;
                for (int y = 0; y < resolution; y++)
                {
                    for (int x = 0; x < resolution; x++)
                    {
                        RectangleViewModel rect = new RectangleViewModel
                        {
                            Row = x,
                            Column = y,
                            Type = TileType.Empty,
                            Width = tileSize * 2d,
                            Height = tileSize * 2d
                        };

                        Tiles.Add(rect);
                    }
                }
            }

            // Color All the Tiles Accordingly
            for (int x = 0; x < resolution; x++)
            {
                for (int y = 0; y < resolution; y++)
                {
                    int index = x + y * resolution;
                    RectangleViewModel rect = Tiles[index];
                    rect.Type = model.Tiles == null ? TileType.Empty : model.Tiles[x][y].type;
                }
            }

            OnPropertyChanged(nameof(ArrowBtnVisibility));
            // CollectionViewSource.GetDefaultView(Tiles).Refresh(); // refresh the colors just to be sure <---- !!!!! this could be a problem !!!!!
        }

        private void ModelUpdateSingleTile(object? sender, TileArgs e)
        {
            Dispatcher.UIThread.Invoke(() =>
            {
                Coordinate? pos = e.coord;
                if (pos == null) return;

                int index = pos.X + Resolution * pos.Y;
                if (index < 0 || index >= Tiles.Count) return;

                this.Tiles[index].Type = model.Tiles![pos.X][pos.Y].type;

                OnPropertyChanged(nameof(ArrowBtnVisibility));
            });
        }

        private void ModelStopGame(object? sender, StopGameArgs e)
        {
            Dispatcher.UIThread.Invoke(() =>
            {
                bool ingame = e.inGame;
                if (!ingame)
                {
                    StartBtnText = "Play";
                    ScoreLabelText = string.Format("Score: {0:000}", model?.Score ?? 0);
                    GameOverLabelText = "Game Over";
                    GameOverLabelColor = new SolidColorBrush(Color.FromRgb(255, 0, 0));
                    ScoreLabelVisibility = true;
                }
                else
                {
                    StartBtnText = "Resume";
                    GameOverLabelText = "Paused";
                    GameOverLabelColor = new SolidColorBrush(Color.FromRgb(255, 140, 0));
                    ScoreLabelVisibility = false;
                }

                MenuBoxVisibility = true;

                OnPropertyChanged(nameof(ArrowBtnVisibility));
            });
        }

        #endregion

        #region View-to-ViewModel(-to-App) Event Methods

        private void OnGamestart()
        {
            StartGame?.Invoke(this, EventArgs.Empty);
            OnPropertyChanged(nameof(ArrowBtnVisibility));
        }

        private void OnLoadMap()
        {
            LoadMap?.Invoke(this, EventArgs.Empty);
        }

        private void OnExitGame()
        {
            ExitGame?.Invoke(this, EventArgs.Empty);
        }

        private void WindowKeyDown(string? key)
        {
            if (key == null) return;

            if (key == "Left")
            {
                model.TurnLeft();
            }
            else if (key == "Right")
            {
                model.TurnRight();
            }
            else if (key == "Esc")
            {
                PauseGame();
            }
        }

        #endregion
    }
}
