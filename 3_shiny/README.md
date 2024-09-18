# shiny app code


This code is used to visualise the results

## Launch the shiny

Follow the next steps to launch the shiny:

1.  Download locally the project:
    <https://github.com/BartsBoneJointHealth/HIPSTAR/archive/refs/heads/main.zip>

2.  Open the project locally: open the
    [3_shiny.Rproj](https://github.com/BartsBoneJointHealth/HIPSTAR/blob/main/3_shiny/3_shiny.Rproj)
    file.

> [!TIP]
>
> The project is correctly open if you can see in the top right of your
> RStudio session this:
> ![](https://github.com/BartsBoneJointHealth/HIPSTAR/blob/main/3_shiny/images/project.png?raw=true)

3.  Restore the library to do so run: `renv::restore()` in the console.

> [!NOTE]
>
> You need to install specific versions of packages as packages can
> change over time, that’s what renv does for you.

4. Move your results file (.csv) to the data folder.

5.  Launch app, to launch the app you can click the ‘Run App’ button on
    the top right or run `shiny::runApp()` in the console. The shiny
    should pop-up in a new window.

> [!WARNING]
>
> If you have any problem running the shiny open an issue:
> <https://github.com/BartsBoneJointHealth/HIPSTAR/issues/new>.
