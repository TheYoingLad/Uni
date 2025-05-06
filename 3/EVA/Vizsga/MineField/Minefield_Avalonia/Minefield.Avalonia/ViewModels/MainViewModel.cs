using Minefield.Model;
using System.Collections.ObjectModel;
using System;
using CommunityToolkit.Mvvm.Input;
using System.Linq;

namespace Minefield.Avalonia.ViewModels;

public partial class MainViewModel : ViewModelBase
{
    #region Fields

    // modell
    private MinefieldGameModel _model;

    #endregion

    #region Properties

    /// <summary>
    /// Lekérdezhető játékmező gyűjtemény
    /// </summary>
    public ObservableCollection<MinefieldGameField> GameFields { get; set; }

    /// <summary>
    /// Játék szünetelésének lekérdezése
    /// </summary>
    public bool IsPaused
    {
        get => _model.IsGamePaused;
    }

    /// <summary>
    /// Játék futásának lekérdezése
    /// </summary>
    public bool IsRunning
    {
        get => _model.IsGameRunning;
    }

    /// <summary>
    /// Játékidő lekérdezése
    /// </summary>
    public string GameTime
    {
        get => _model.GameTime.ToString(@"hh\:mm\:ss");
    }

    #endregion

    #region Commands

    /// <summary>
    /// Új játék parancs
    /// </summary>
    public RelayCommand NewGameCommand { get; private set; }

    /// <summary>
    /// Játék betöltése parancs
    /// </summary>
    public RelayCommand LoadGameCommand { get; private set; }

    /// <summary>
    /// Játék mentése parancs
    /// </summary>
    public RelayCommand SaveGameCommand { get; private set; }

    /// <summary>
    /// W billenyű megnyomása parancs
    /// </summary>
    public RelayCommand KeyPressedWCommand { get; private set; }

    /// <summary>
    /// S billenyű megnyomása parancs
    /// </summary>
    public RelayCommand KeyPressedSCommand { get; private set; }

    /// <summary>
    /// A billenyű megnyomása parancs
    /// </summary>
    public RelayCommand KeyPressedACommand { get; private set; }

    /// <summary>
    /// D billenyű megnyomása parancs
    /// </summary>
    public RelayCommand KeyPressedDCommand { get; private set; }

    /// <summary>
    /// Szóköz billenyű megnyomása parancs
    /// </summary>
    public RelayCommand KeyPressedSpaceCommand { get; private set; }

    #endregion

    #region Events

    /// <summary>
    /// Új játék esemény
    /// </summary>
    public event EventHandler? NewGame;

    /// <summary>
    /// Játék mentése esemény
    /// </summary>
    public event EventHandler? SaveGame;

    /// <summary>
    /// Játék betöltése esemény
    /// </summary>
    public event EventHandler? LoadGame;

    /// <summary>
    /// Mozgást irányító billentyű (WASD) megnyomása esemény
    /// </summary>
    public event EventHandler<Direction>? MovementKeyPressed;

    /// <summary>
    /// Szóköz billentyű megnyomása esemény
    /// </summary>
    public event EventHandler? KeyPressedSpace;

    #endregion

    #region Constructor

    /// <summary>
    /// Aknamező nézetmodellt példányosító konstruktor
    /// </summary>
    /// <param name="model">Aknamező játék modell</param>
    public MainViewModel(MinefieldGameModel model)
    {
        _model = model;
        _model.TimeAdvanced += new EventHandler<MinefieldEventArgs>(Model_TimeAdvanced);
        _model.CellUpdate += new EventHandler<MinefieldCellUpdateEventArgs>(Model_CellUpdate);
        _model.GameOver += new EventHandler<MinefieldEventArgs>(Model_GameOver);

        GameFields = new ObservableCollection<MinefieldGameField>();
        for (int i = 0; i < _model._gridRows; i++)
            for (int j = 0; j < _model._gridColumns; j++)
                GameFields.Add(new MinefieldGameField(i, j));

        NewGameCommand = new RelayCommand(OnNewGame);
        SaveGameCommand = new RelayCommand(OnSaveGame);
        LoadGameCommand = new RelayCommand(OnLoadGame);
        KeyPressedWCommand = new RelayCommand(() => OnMoveKeyPressed(Direction.Up));
        KeyPressedSCommand = new RelayCommand(() => OnMoveKeyPressed(Direction.Down));
        KeyPressedACommand = new RelayCommand(() => OnMoveKeyPressed(Direction.Left));
        KeyPressedDCommand = new RelayCommand(() => OnMoveKeyPressed(Direction.Right));
        KeyPressedSpaceCommand = new RelayCommand(OnKeyPressedSpace);
    }

    #endregion

    #region VM Event invocations

    /// <summary>
    /// Új játék esemény kiváltása
    /// </summary>
    private void OnNewGame()
    {
        NewGame?.Invoke(this, EventArgs.Empty);
        OnPropertyChanged(nameof(IsRunning));
        OnPropertyChanged(nameof(IsPaused));
    }

    /// <summary>
    /// Játék mentése esemény kiváltása
    /// </summary>
    private void OnSaveGame()
    {
        SaveGame?.Invoke(this, EventArgs.Empty);
        OnPropertyChanged(nameof(IsPaused));
    }

    /// <summary>
    /// Játék betöltése esemény kiváltása
    /// </summary>
    private void OnLoadGame()
    {
        LoadGame?.Invoke(this, EventArgs.Empty);
        OnPropertyChanged(nameof(IsRunning));
        OnPropertyChanged(nameof(IsPaused));
    }

    /// <summary>
    /// Mozgást irányító esemény kiváltása
    /// </summary>
    /// <param name="d">Mozgás iránya</param>
    private void OnMoveKeyPressed(Direction d) => MovementKeyPressed?.Invoke(this, d);

    /// <summary>
    /// Szóköz megnyomására a játékot szüneteltető esemény kiváltása
    /// </summary>
    private void OnKeyPressedSpace()
    {
        KeyPressedSpace?.Invoke(this, EventArgs.Empty);
        OnPropertyChanged(nameof(IsPaused));
        OnPropertyChanged(nameof(IsRunning));
    }
    #endregion

    #region Model event handlers

    /// <summary>
    /// Játék vége eseménykezelő
    /// </summary>
    private void Model_GameOver(object? sender, MinefieldEventArgs e)
    {
        OnPropertyChanged(nameof(IsRunning));
        OnPropertyChanged(nameof(IsPaused));
    }

    /// <summary>
    /// Cella változása eseménykezelő
    /// </summary>
    private void Model_CellUpdate(object? sender, MinefieldCellUpdateEventArgs e)
    {
        MinefieldGameField thisField = GameFields.Single(x => x.X == e.X && x.Y == e.Y);
        if (_model[e.X, e.Y] == null)
        {
            thisField.NotEmpty = false;
            thisField.Entity = null;
        }
        else
        {
            thisField.NotEmpty = true;
            thisField.Entity = _model[e.X, e.Y];
        }
    }

    /// <summary>
    /// Játékidő előrehaladása eseménykezelő
    /// </summary>
    private void Model_TimeAdvanced(object? sender, MinefieldEventArgs e) => OnPropertyChanged(nameof(GameTime));

    #endregion
}
