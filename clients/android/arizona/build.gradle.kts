plugins {
    alias(libs.plugins.android.library)
    alias(libs.plugins.kotlin.android)
    alias(libs.plugins.kotlin.serialization)
    alias(libs.plugins.compose.compiler)
}

android {
    namespace = "dev.arizona.client"
    compileSdk = 35

    defaultConfig {
        minSdk = 24
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

    testOptions {
        unitTests {
            // The JVM unit tests drive AzClient's *op application* (handleText),
            // which is pure -- but constructing the client touches the android.jar
            // stubs (Handler/Looper for the main-thread marshalling). Returning
            // defaults instead of throwing lets those tests exist without an
            // emulator; the Handler-scheduled paths (heartbeat, reconnect backoff)
            // are no-ops there and stay covered by the on-device e2e.
            isReturnDefaultValues = true
        }
    }
}

dependencies {
    implementation(libs.okhttp)
    implementation(libs.kotlinx.serialization.json)

    val composeBom = platform(libs.androidx.compose.bom)
    implementation(composeBom)
    implementation(libs.androidx.runtime) // mutableStateOf, snapshot state
    implementation(libs.androidx.ui)

    // Pure-logic unit tests (interleave, op application) -- run on the JVM,
    // no emulator needed: ./gradlew :arizona:testDebugUnitTest
    testImplementation(libs.junit)
    testImplementation(libs.kotlinx.serialization.json)
}
