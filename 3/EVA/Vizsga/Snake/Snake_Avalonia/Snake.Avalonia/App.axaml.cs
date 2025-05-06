using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Data.Core.Plugins;
using Avalonia.Markup.Xaml;
using Avalonia.Platform;
using Avalonia.Platform.Storage;
using DialogHostAvalonia;
using MsBox.Avalonia;
using MsBox.Avalonia.Dto;
using MsBox.Avalonia.Enums;
using Snake.Avalonia.ViewModels;
using Snake.Avalonia.Views;
using Snake.Model;
using Snake.Persistence;
using System;
using System.Diagnostics;
using System.IO;
using System.Threading;

namespace Snake.Avalonia;

public partial class App : Application, IDisposable
{

    #region Fields

    private GameModel model = null!;
    private MainViewModel viewModel = null!;

    #endregion

    #region Properties

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

    #region Application methods

    public override void Initialize()
    {
        AvaloniaXamlLoader.Load(this);
    }

    public override void OnFrameworkInitializationCompleted()
    {
        // Line below is needed to remove Avalonia data validation.
        // Without this line you will get duplicate validations from both Avalonia and CT
        BindingPlugins.DataValidators.RemoveAt(0);

        model = new GameModel();

        viewModel = new MainViewModel(model);
        viewModel.StartGame += ViewModelStartGame;
        viewModel.LoadMap += ViewModelLoadMap;
        viewModel.ExitGame += ViewModelExitGame;


        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            desktop.MainWindow = new MainWindow
            {
                DataContext = viewModel
            };

            desktop.MainWindow.WindowStartupLocation = WindowStartupLocation.CenterScreen;
        }
        else if (ApplicationLifetime is ISingleViewApplicationLifetime singleViewPlatform)
        {
            singleViewPlatform.MainView = new MainView
            {
                DataContext = viewModel
            };

            if (Application.Current?.TryGetFeature<IActivatableLifetime>() is { } activatableLifetime)
            {
                activatableLifetime.Deactivated += (sender, args) =>
                {
                    if (args.Kind == ActivationKind.Background)
                    {
                        viewModel.PauseGame();
                    }
                };
            }
        }

        base.OnFrameworkInitializationCompleted();

        Thread thread = new Thread(async () =>
        {
            await model.ReloadModel(null, null, true);
        });
        thread.Start();
        thread.Join();
    }

    #endregion

    #region ViewModel-to-App Event Methods

    // Runs when Play Button is pressed
    private async void ViewModelStartGame(object? sender, EventArgs e)
    {
        if (model.IsWallsEmpty && !model.Ingame)
        {
            // If no model is present then ask whether to play on an empty map is wanted
            var msgbox = MessageBoxManager.GetMessageBoxStandard("Warning",
                                                                 "No map has been loaded in. Are you sure to play on an empty map?",
                                                                 ButtonEnum.YesNo);
            var result = await msgbox.ShowAsync();            

            if (result == ButtonResult.Yes)
            {
                await model.ReloadModel();

                viewModel.MenuBoxVisibility = false;
                model.StartGame(newGame: true);
            }
        }
        else
        {
            if (!model.Ingame) model.ResetGame();

            viewModel.MenuBoxVisibility = false;
            model.StartGame(newGame: !model.Ingame);
        }
    }

    // Runs when Load Map Button is pressed
    private async void ViewModelLoadMap(object? sender, EventArgs e)
    {
        if (TopLevel == null)
        {
            await MessageBoxManager.GetMessageBoxStandard(
                    "ERROR",
                    "Filehandling is not supported!",
                    ButtonEnum.Ok, Icon.Error)
                .ShowAsync();
            return;
        }

        try
        {
            var files = await TopLevel.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
            {
                Title = "Snake Map Injection",
                AllowMultiple = false,
                FileTypeFilter = new[]
                {
                    new FilePickerFileType("Save file")
                    {
                        Patterns = new[] { "*.save" }
                    },
                    new FilePickerFileType("All files")
                    {
                        Patterns = new[] { "*.*" }
                    }
                }
            });

            if (files.Count > 0)
            {
                using (Stream stream = await files[0].OpenReadAsync())
                {
                    IDataAccess dataAccess = new SaveFileDataAccess(stream);
                    await model.ReloadModel(dataAccess);
                }

                viewModel.MapLoadingViewChange();
            }
        }
        catch (Exception ex)
        {
            var msgbox = MessageBoxManager.GetMessageBoxStandard("ERROR", $"Error loading map in: {ex.Message}",
                                                                      ButtonEnum.Ok, Icon.Error, WindowStartupLocation.CenterScreen);
            await msgbox.ShowAsync();
        }
    }

    // Runs when Exit Button is pressed
    private void ViewModelExitGame(object? sender, EventArgs e)
    {
        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            desktop.MainWindow?.Close();
        }
    }

    #endregion

    void IDisposable.Dispose()
    {
        model?.Dispose();
    }
}
