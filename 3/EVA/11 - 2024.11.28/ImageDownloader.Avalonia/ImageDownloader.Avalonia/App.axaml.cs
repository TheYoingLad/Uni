using Avalonia;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Data.Core.Plugins;
using Avalonia.Markup.Xaml;
using Avalonia.Media.Imaging;
using ImageDownloader.Avalonia.ViewModels;
using ImageDownloader.Avalonia.Views;
using MsBox.Avalonia;
using System;

namespace ImageDownloader.Avalonia;

public partial class App : Application
{
    private MainViewModel _mainViewModel = null!;
    private MainView _mainView = null!;
    public override void Initialize()
    {
        AvaloniaXamlLoader.Load(this);
    }

    public override void OnFrameworkInitializationCompleted()
    {
        _mainViewModel = new MainViewModel();
        _mainViewModel.ErrorOccured += OnErrorOccured;
        _mainViewModel.ImageSelected += OnImageSelected;

        // Line below is needed to remove Avalonia data validation.
        // Without this line you will get duplicate validations from both Avalonia and CT
        BindingPlugins.DataValidators.RemoveAt(0);

        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            desktop.MainWindow = new MainWindow
            {
                DataContext = _mainViewModel
            };
        }
        else if (ApplicationLifetime is ISingleViewApplicationLifetime singleViewPlatform)
        {
            singleViewPlatform.MainView = new MainView
            {
                DataContext = _mainViewModel
            };
        }

        base.OnFrameworkInitializationCompleted();
    }

    private void OnErrorOccured(object? sender, string message)
    {
        MessageBoxManager.GetMessageBoxStandard("Error occured", message, MsBox.Avalonia.Enums.ButtonEnum.Ok, MsBox.Avalonia.Enums.Icon.Error);
    }

    private void OnImageSelected(object? sender, Bitmap e)
    {
        var imageViewModel = new ImageViewModel(e);

        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            var imageWindow = new ImageWindow
            {
                DataContext = imageViewModel
            };
            imageWindow.Show();
        }
        else if (ApplicationLifetime is ISingleViewApplicationLifetime singleViewPlatform)
        {
            imageViewModel.CloseImage += (object? _, EventArgs _) =>
            {
                //singleViewPlatform = _mainView;
            };
            singleViewPlatform.MainView = new ImageView
            {
                DataContext = imageViewModel
            };
        }
    }
}
