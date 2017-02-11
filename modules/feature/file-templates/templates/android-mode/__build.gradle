# -*- mode: snippet -*-
# name: build.gradle template
# condition: (eq major-mode 'groovy-mode)
# --
buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath 'com.android.tools.build:gradle:1.2.3'
    }
}

apply plugin: 'android'

android {
    compileSdkVersion 22
    buildToolsVersion "22.0.1"

    sourceSets {
        main {
            manifest.srcFile 'AndroidManifest.xml'
            java.srcDirs = ['java']
            resources.srcDirs = ['java']
            aidl.srcDirs = ['java']
            renderscript.srcDirs = ['java']
            res.srcDirs = ['res']
            assets.srcDirs = ['assets']
        }

        // Move the tests to tests/java, tests/res, etc...
        androidTest.setRoot('../tests')

        // Move the build types to build-types/<type>
        // For instance, build-types/debug/java, build-types/debug/AndroidManifest.xml, ...
        // This moves them out of them default location under src/<type>/... which would
        // conflict with src/ being used by the main source set.
        // Adding new build types or product flavors should be accompanied
        // by a similar customization.
        debug.setRoot('build-types/debug')
        release.setRoot('build-types/release')
    }

    lintOptions {
        abortOnError false
    }
}

$0
