
workspace "My"
	architecture "x64"
	startproject "Scripting"

	configurations {
		"Debug",
		"Release"
	}


OutputDir = "%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"

project "My"
	location "My"
	kind "StaticLib"
	language "C++"
	cppdialect "C++17"
	staticruntime "on"

	targetdir ("bin/" .. OutputDir .. "/%{prj.name}")
	objdir    ("bin-int/" .. OutputDir .. "/%{prj.name}")
	
	files {
		"%{prj.name}/Source/**.h",
		"%{prj.name}/Source/**.cpp",
		"%{prj.name}/Vendor/**.h",
		"%{prj.name}/Vendor/**.c",
		"%{prj.name}/Vendor/**.hpp",
		"%{prj.name}/Vendor/**.cpp",
	}
	
	includedirs {
		"%{prj.name}/Source",
		"%{prj.name}/Vendor",
	}

	filter "system:windows"
		systemversion "latest"
		defines {
			"MY_WIN32",
		}
		
	filter "system:linux"
		defines {
			"MY_LINUX",
		}

	filter "configurations:Debug"
		defines "MY_DEBUG"
		symbols "on"
		
	filter "configurations:Release"
		defines "MY_RELEASE"
		optimize "on"


project "Sandbox"
	location "Sandbox"
	kind "ConsoleApp"
	language "C++"
	cppdialect "C++17"
	staticruntime "on"

	targetdir ("bin/" .. OutputDir .. "/%{prj.name}")
	objdir    ("bin-int/" .. OutputDir .. "/%{prj.name}")

	files {
		"%{prj.name}/Main.cpp",
	}
	
	includedirs {
		"My/Source",
		"My/Vendor",
	}
	
	links {
		"My" 
	}
	
	filter "system:windows"
		systemversion "latest"
		defines {
			"MY_WIN32"
		}
	
	filter "system:linux"
		defines {
			"MY_LINUX",
		}

	filter "configurations:Debug"
		defines "MY_DEBUG"
		symbols "on"
		
	filter "configurations:Release"
		defines "MY_RELEASE"
		optimize "on"
		

project "Scripting"
	location "Scripting"
	kind "ConsoleApp"
	language "C++"
	cppdialect "C++17"
	staticruntime "on"

	targetdir ("bin/" .. OutputDir .. "/%{prj.name}")
	objdir    ("bin-int/" .. OutputDir .. "/%{prj.name}")

	files {
		"%{prj.name}/Main.cpp",
	}
	
	includedirs {
		"My/Source",
		"My/Vendor",
	}
	
	links {
		"My" 
	}
	
	filter "system:windows"
		systemversion "latest"
		defines {
			"MY_WIN32"
		}
	
	filter "system:linux"
		defines {
			"MY_LINUX",
		}

	filter "configurations:Debug"
		defines "MY_DEBUG"
		symbols "on"
		
	filter "configurations:Release"
		defines "MY_RELEASE"
		optimize "on"
