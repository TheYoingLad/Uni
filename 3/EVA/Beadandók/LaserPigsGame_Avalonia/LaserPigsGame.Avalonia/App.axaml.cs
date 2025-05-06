using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Data.Core.Plugins;
using Avalonia.Markup.Xaml;
using Avalonia.Platform.Storage;
using LaserPigs.Model;
using LaserPigs.Persistence;
using LaserPigsGame.Avalonia.View;
using LaserPigsGame.Avalonia.ViewModel;
using MsBox.Avalonia;
using MsBox.Avalonia.Enums;
using System;

namespace LaserPigsGame.Avalonia;

public partial class App : Application
{
    #region Fields
    private LaserPigsModel _model = null!;
    private LaserPigsViewModel _viewModel = null!;
    #endregion

    #region Properites
    private TopLevel? TopLevel
    {
        get
        {
            return ApplicationLifetime switch
            {
                IClassicDesktopStyleApplicationLifetime desktop => TopLevel.GetTopLevel(desktop.MainWindow),
                ISingleViewApplicationLifetime singleViewPlatform => TopLevel.GetTopLevel(singleViewPlatform.MainView),
                _ => null
            };
        }
    }
    #endregion

    #region App Methods
    public override void Initialize()
    {
        AvaloniaXamlLoader.Load(this);
    }
    public override void OnFrameworkInitializationCompleted()
    {
        // Line below is needed to remove Avalonia data validation.
        // Without this line you will get duplicate validations from both Avalonia and CT
        BindingPlugins.DataValidators.RemoveAt(0);

        _model = new LaserPigsModel(new FileManager());
        _model.GameOver += Model_GameOver;
        _model.NewGame();

        _viewModel = new LaserPigsViewModel(_model);
        _viewModel.NewGame += ViewModel_NewGame;
        _viewModel.LoadGame += ViewModel_LoadGame;
        _viewModel.SaveGame += ViewModel_SaveGame;
        _viewModel.ExitGame += ViewModel_ExitGame;

        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            desktop.MainWindow = new MainWindow
            {
                DataContext = _viewModel
            };
        }
        else if (ApplicationLifetime is ISingleViewApplicationLifetime singleViewPlatform)
        {
            singleViewPlatform.MainView = new MainView
            {
                DataContext = _viewModel
            };
        }

        base.OnFrameworkInitializationCompleted();
    }
    #endregion

    #region Model event handlers
    private async void Model_GameOver(object? sender, GameOverEventArgs e)
    {
        switch (e.GetWinner)
        {
            case true:
                await MessageBoxManager.GetMessageBoxStandard("Game Over", "Player 1 won!", ButtonEnum.Ok, Icon.Info).ShowAsync();
                break;
            case false:
                await MessageBoxManager.GetMessageBoxStandard("Game Over", "Player 2 won!", ButtonEnum.Ok, Icon.Info).ShowAsync();
                break;
            default:
                await MessageBoxManager.GetMessageBoxStandard("Game Over", "It's a Draw!", ButtonEnum.Ok, Icon.Info).ShowAsync();
                break;
        }

        await MessageBoxManager.GetMessageBoxStandard("New Game", "Starting new game...", ButtonEnum.Ok, Icon.Info).ShowAsync();
        _model.NewGame();
    }
    #endregion

    #region ViewModel event handlers
    private async void ViewModel_NewGame(object? sender, int n)
    {
        if (await MessageBoxManager.GetMessageBoxStandard("New Game", $"Start new game?\n\nNew grid size: {n}", ButtonEnum.YesNo, Icon.Question).ShowAsync() == ButtonResult.Yes)
        {
            _model.Size = n;
            _model.NewGame();
        }
    }
    private async void ViewModel_LoadGame(object? sender, EventArgs e)
    {
        if (TopLevel == null)
        {
            await MessageBoxManager.GetMessageBoxStandard("Error", "An error occured:\nFile management is not supported", ButtonEnum.Ok, Icon.Error).ShowAsync();
            return;
        }

        try
        {
            var file = await TopLevel.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
            {
                Title = "Load Laser Pigs File",
                AllowMultiple = false,
                FileTypeFilter = new[]
                {
                    new FilePickerFileType("Laser Pigs File")
                    {
                        Patterns = new[] { "*.lspf" }
                    }
                }
            });

            if (file.Count > 0)
            {
                using (var stream = await file[0].OpenReadAsync()) await _model.LoadGameAsync(stream);
            }
        }
        catch (Exception ex)
        {
            await MessageBoxManager.GetMessageBoxStandard("Error", "An error occured:\n" + ex.Message, ButtonEnum.Ok, Icon.Error).ShowAsync();
            await MessageBoxManager.GetMessageBoxStandard("Restart", "File could not be loaded, restarting game", ButtonEnum.Ok, Icon.Info).ShowAsync();

            _model.NewGame();
        }
    }
    private async void ViewModel_SaveGame(object? sender, EventArgs e)
    {
        if (TopLevel == null)
        {
            await MessageBoxManager.GetMessageBoxStandard("An error occured", "File management is not supported", ButtonEnum.Ok, Icon.Error).ShowAsync();
            return;
        }

        try
        {
            var file = await TopLevel.StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions()
            {
                Title = "Save Laser Pig File",
                FileTypeChoices = new[]
                {
                    new FilePickerFileType("Laser Pig File")
                    {
                        Patterns = new[] { "*.lspf" }
                    }
                }
            });

            if (file != null)
            {
                using (var stream = await file.OpenWriteAsync()) await _model.SaveGameAsync(stream);
            }
        }
        catch (Exception ex)
        {
            await MessageBoxManager.GetMessageBoxStandard("Error", "An error occured:\n" + ex.Message, ButtonEnum.Ok, Icon.Error).ShowAsync();
        }
    }
    private async void ViewModel_ExitGame(object? sender, EventArgs e)
    {
        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop && await MessageBoxManager.GetMessageBoxStandard("Exit", "Are your sure you want to quit?", ButtonEnum.YesNo, Icon.Question).ShowAsync() == ButtonResult.Yes) desktop.Shutdown();
    }
    #endregion
}
