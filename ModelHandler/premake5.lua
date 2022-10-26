workspace "ModelHandler"

	architecture "x64"

	configurations
	{
		"Debug",
		"Release",
	}

	startproject  "ModelHandler"

outputdir = "%{cfg.buildcfg}-%{cfg.system}"

project "ModelHandler"
    location "ModelHandler"
    kind "ConsoleApp"
    language "C#"

    targetdir("bin/" .. outputdir .. "/%{prj.name}")
	objdir("bin-int/" .. outputdir .. "/%{prj.name}")

    nuget
    {
        "System.Data.SQLite:1.0.116",
    }

    links { "System", "System.Data" }

    files { "%{prj.name}/src/**.cs" }

    filter "configurations:Debug"
		runtime "Debug"
		symbols "on"

    filter "configurations:Release"
		runtime "Release"
		optimize "on"