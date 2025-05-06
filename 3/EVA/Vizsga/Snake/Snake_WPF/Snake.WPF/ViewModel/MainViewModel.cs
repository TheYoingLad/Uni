using Snake.Model;
using System.Collections.ObjectModel;
using System.Windows;
using System.Windows.Data;
using System.Windows.Media;

namespace Snake.WPF.ViewModel
{
    public class MainViewModel : ViewModelBase
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
        private Brush gameOverLabelColor = Brushes.Red;

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

        // Delegate Commands (View-to-ViewModel Communication)
        public DelegateCommand PlayCommand { get; private set; }
        public DelegateCommand LoadCommand { get; private set; }
        public DelegateCommand ExitCommand { get; private set; }
        public DelegateCommand KeyDownCommand { get; private set; }

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
            PlayCommand = new DelegateCommand(param => OnGamestart());
            LoadCommand = new DelegateCommand(param => OnLoadMap());
            ExitCommand = new DelegateCommand(param => OnExitGame());
            KeyDownCommand = new DelegateCommand(param => WindowKeyDown(param as string));
        }

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

            CollectionViewSource.GetDefaultView(Tiles).Refresh(); // refresh the colors
        }

        private void ModelUpdateSingleTile(object? sender, TileArgs e)
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                Coordinate? pos = e.coord;
                if (pos == null) return;

                int index = pos.X + Resolution * pos.Y;
                if (index < 0 || index >= Tiles.Count) return;

                this.Tiles[index].Type = model.Tiles![pos.X][pos.Y].type;
            });
        }

        private void ModelStopGame(object? sender, StopGameArgs e)
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                bool ingame = e.inGame;
                if (!ingame)
                {
                    StartBtnText = "Play";
                    ScoreLabelText = string.Format("Score: {0:000}", model?.Score ?? 0);
                    GameOverLabelText = "Game Over";
                    GameOverLabelColor = Brushes.Red;
                    ScoreLabelVisibility = true;
                }
                else
                {
                    StartBtnText = "Resume";
                    GameOverLabelText = "Paused";
                    GameOverLabelColor = Brushes.DarkOrange;
                    ScoreLabelVisibility = false;
                }

                MenuBoxVisibility = true;
            });
        }

        #endregion

        #region View-to-ViewModel(-to-App) Event Methods

        private void OnGamestart()
        {
            StartGame?.Invoke(this, EventArgs.Empty);
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
                if (!MenuBoxVisibility) model.StopGame(gameOver: false);
                else if (model.Ingame)
                {
                    MenuBoxVisibility = false;
                    model.StartGame(newGame: !model.Ingame);
                }
            }
        }

        #endregion
    }
}
