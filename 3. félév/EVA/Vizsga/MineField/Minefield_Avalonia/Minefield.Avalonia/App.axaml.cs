using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Data.Core.Plugins;
using Avalonia.Markup.Xaml;

using Minefield.Avalonia.ViewModels;
using Minefield.Avalonia.Views;
using Minefield.Model;
using Minefield.Persistence;
using System;
using System.IO;
using Avalonia.Platform;
using MsBox.Avalonia;
using MsBox.Avalonia.Enums;
using Avalonia.Platform.Storage;
using System.Collections.Generic;
using System.Linq;
using Avalonia.Threading;

namespace Minefield.Avalonia;

public partial class App : Application, IDisposable
{

    private MinefieldGameModel _model = null!;
    private MainViewModel _viewModel = null!;

    private TopLevel? TopLevel
    {
        get
        {
            return ApplicationLifetime switch
            {
                IClassicDesktopStyleApplicationLifetime desktop =>
                    TopLevel.GetTopLevel(desktop.MainWindow),

                ISingleViewApplicationLifetime singleViewPlatform =>
                    TopLevel.GetTopLevel(singleViewPlatform.MainView),

                _ =>
                    null
            };
        }
    }



    public override void Initialize()
    {
        AvaloniaXamlLoader.Load(this);
    }

    public override void OnFrameworkInitializationCompleted()
    {
        BindingPlugins.DataValidators.RemoveAt(0);

        _model = new MinefieldGameModel(new MinefieldFileAccess());
        _model.GameOver += new System.EventHandler<MinefieldEventArgs>(Model_GameOver);

        _viewModel = new MainViewModel(_model);
        _viewModel.NewGame += new EventHandler(ViewModel_NewGame);
        _viewModel.SaveGame += new EventHandler(ViewModel_SaveGame);
        _viewModel.LoadGame += new EventHandler(ViewModel_LoadGame);
        _viewModel.MovementKeyPressed += new EventHandler<Direction>(ViewModel_MovementKeyPressed);
        _viewModel.KeyPressedSpace += new EventHandler(ViewModel_KeyPressedSpace);

        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            desktop.MainWindow = new MainWindow
            {
                DataContext = _viewModel
            };
            desktop.Startup += async (_, _) =>
            {
                try
                {
                    await _model.LoadGameAsync(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "autosave"));
                }
                catch
                {
                    
                }
            };
            desktop.Exit += async (_, _) =>
            {
                try
                {
                    await _model.SaveGameAsync(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "autosave"));
                }
                catch
                {
                    
                }
            };
        }
        else if (ApplicationLifetime is ISingleViewApplicationLifetime singleViewPlatform)
        {
            singleViewPlatform.MainView = new MainView
            {
                DataContext = _viewModel
            };

            if (Application.Current?.TryGetFeature<IActivatableLifetime>() is { } activatableLifetime)
            {
                activatableLifetime.Activated += async (_, eventargs) =>
                {
                    if (eventargs.Kind == ActivationKind.Background)
                    {
                        try
                        {
                            await _model.LoadGameAsync(Path.Combine(AppContext.BaseDirectory, "autosave"));
                        }
                        catch
                        {
                            
                        }
                    }
                };
                activatableLifetime.Deactivated += async (_, eventargs) =>
                {
                    if (eventargs.Kind == ActivationKind.Background)
                    {
                        try
                        {
                            await _model.SaveGameAsync(Path.Combine(AppContext.BaseDirectory, "autosave"));
                        }
                        catch
                        {
                            
                        }
                    }
                };
            }
        }
        base.OnFrameworkInitializationCompleted();
    }





    #region Model event handlers

    /// <summary>
    /// Játék vége eseménykezelő
    /// </summary>
    private async void Model_GameOver(object? sender, MinefieldEventArgs e)
    {
        await Dispatcher.UIThread.Invoke(async () =>
        {
            await MessageBoxManager.GetMessageBoxStandard("Game Over", $"Time survived: {(int)e.Time.TotalMinutes:0}:{e.Time.Seconds:00}.{e.Time.Milliseconds / 100:0}", ButtonEnum.Ok, Icon.Info).ShowAsync();
        });
    }



    #endregion

    #region ViewModel event handlers

    /// <summary>
    /// Szóköz billentyű megnyomása eseménykezelő
    /// </summary>
    private void ViewModel_KeyPressedSpace(object? sender, EventArgs e)
    {
        _model.TogglePaused();
    }

    /// <summary>
    /// Mozgást irányító billentyű megnyomása eseménykezelő
    /// </summary>
    private void ViewModel_MovementKeyPressed(object? sender, Direction d) => _model.MovePlayer(d);

    /// <summary>
    /// Új játék eseménykezelő
    /// </summary>
    private void ViewModel_NewGame(object? sender, EventArgs e)
    {
        _model.StartNewGame();
    }

    /// <summary>
    /// Játék mentése eseménykezelő
    /// </summary>
    private async void ViewModel_SaveGame(object? sender, EventArgs e)
    {
        if (TopLevel is null)
        {
            await MessageBoxManager.GetMessageBoxStandard("Error", "Operation failed.", ButtonEnum.Ok, Icon.Error).ShowAsync();
            return;
        }




        try
        {
            IStorageFile? file = await TopLevel.StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions()
            {
                Title = "Save game",
                FileTypeChoices = new[] { new FilePickerFileType("Minefield Save File") { Patterns = new[] { "*.mfs" } } }
            }
            );
            if (file is not null)
            {
                using (Stream? stream = await file.OpenWriteAsync())
                {
                    await _model.SaveGameAsync(stream);
                }
            }
        }
        catch (Exception ex)
        {
            await MessageBoxManager.GetMessageBoxStandard("Error", $"Operation Failed. Details: {ex.Message}", ButtonEnum.Ok, Icon.Error).ShowAsync();
        }
    }

    /// <summary>
    /// Játék betöltése eseménykezelő
    /// </summary>
    private async void ViewModel_LoadGame(object? sender, EventArgs e)
    {
        if (TopLevel is null)
        {
            // replace text
            await MessageBoxManager.GetMessageBoxStandard("Error", "Opertion failed.", ButtonEnum.Ok, Icon.Error).ShowAsync();
            return;
        }

        try
        {
            IReadOnlyList<IStorageFile> files = await TopLevel.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions()
            {
                Title = "Save game",
                AllowMultiple = false,
                FileTypeFilter = new[] { new FilePickerFileType("Minefield Save File") { Patterns = new[] { "*.mfs" } } }
            }
            );

            if (files.Any())
            {
                using (Stream? stream = await files[0].OpenReadAsync())
                {
                    await _model.LoadGameAsync(stream);
                }
            }
        }
        catch (Exception ex)
        {
            await MessageBoxManager.GetMessageBoxStandard("Error", $"Operation failed. Details: {ex.Message}", ButtonEnum.Ok, Icon.Error).ShowAsync();
        }
    }
    #endregion
    
    public void Dispose()
    {
        _model.Dispose();
    }

}
