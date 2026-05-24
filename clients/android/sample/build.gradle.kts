import java.util.Properties

plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.android)
    alias(libs.plugins.compose.compiler)
}

android {
    namespace = "dev.arizona.sample"
    compileSdk = 35

    defaultConfig {
        applicationId = "dev.arizona.sample"
        minSdk = 24
        targetSdk = 35
        versionCode = 1
        versionName = "0.1.0"
        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    buildFeatures {
        compose = true
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
    kotlinOptions {
        jvmTarget = "17"
    }
}

dependencies {
    implementation(project(":arizona"))

    implementation(libs.androidx.core.ktx)
    implementation(libs.androidx.lifecycle.runtime.ktx)
    implementation(libs.androidx.activity.compose)

    val composeBom = platform(libs.androidx.compose.bom)
    implementation(composeBom)
    implementation(libs.androidx.ui)
    implementation(libs.androidx.foundation)
    implementation(libs.androidx.material3)

    // Instrumented (emulator) e2e: ./gradlew :sample:connectedCheck
    androidTestImplementation(composeBom)
    androidTestImplementation(libs.kotlinx.serialization.json) // build event payloads
    androidTestImplementation(libs.androidx.junit)
    androidTestImplementation(libs.androidx.espresso.core)
    androidTestImplementation(libs.androidx.ui.test.junit4)
    debugImplementation(libs.androidx.ui.test.manifest)
}

// Tunnel the device's localhost:4040 to the host's Arizona server after every
// install, so you never run `adb reverse` by hand -- Android Studio's Run
// installs, so its run loop is covered too. (adb reverse resets on reconnect/
// reboot/adb restart; re-running it on each install keeps it set.) Harmless if
// there's no device or the tunnel already exists.
val adbReverse by tasks.registering(Exec::class) {
    val sdkDir = System.getenv("ANDROID_HOME")
        ?: System.getenv("ANDROID_SDK_ROOT")
        ?: rootProject.file("local.properties").takeIf { it.exists() }?.let { f ->
            Properties().apply { f.inputStream().use { load(it) } }.getProperty("sdk.dir")
        }
    commandLine(sdkDir?.let { "$it/platform-tools/adb" } ?: "adb", "reverse", "tcp:4040", "tcp:4040")
    isIgnoreExitValue = true
}
tasks.matching { it.name.startsWith("install") }.configureEach {
    finalizedBy(adbReverse)
}
