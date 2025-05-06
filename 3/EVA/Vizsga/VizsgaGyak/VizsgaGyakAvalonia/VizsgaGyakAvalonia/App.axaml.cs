using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Data.Core.Plugins;
using Avalonia.Markup.Xaml;
using Avalonia.Platform;
using System;
using System.IO;
using System.Threading.Tasks;
using VizsgaGyakAvalonia.Model;
using VizsgaGyakAvalonia.ViewModels;
using VizsgaGyakAvalonia.Views;

namespace VizsgaGyakAvalonia;

public partial class App : Application
{
    private InvasionModel _model = null!;
    private InvasionViewModel _viewModel = null!;

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

    public override void Initialize()
    {
        AvaloniaXamlLoader.Load(this);
    }
    public override void OnFrameworkInitializationCompleted()
    {
        // Line below is needed to remove Avalonia data validation.
        // Without this line you will get duplicate validations from both Avalonia and CT
        BindingPlugins.DataValidators.RemoveAt(0);

        _model = new InvasionModel();

        _viewModel = new InvasionViewModel(_model);

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
                    await Task.Run(() => /*load*/Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "autosave"));
                }
                catch
                {
                }
            };
            desktop.Exit += async (_, _) =>
            {
                try
                {
                    await Task.Run(() => /*save*/Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "autosave"));
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
                activatableLifetime.Activated += async (_, args) =>
                {
                    if (args.Kind == ActivationKind.Background)
                    {
                        try
                        {
                            await Task.Run(() => /*load*/Path.Combine(AppContext.BaseDirectory, "autosave"));
                        }
                        catch
                        {
                        }
                    }
                };

                activatableLifetime.Deactivated += async (_, args) =>
                {
                    if (args.Kind == ActivationKind.Background)
                    {
                        try
                        {
                            await Task.Run(() => /*save*/Path.Combine(AppContext.BaseDirectory, "autosave"));
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
}
