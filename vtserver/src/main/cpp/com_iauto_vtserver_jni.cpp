
/**
 * Copyright @ 2020 - 2021 iAUTO(Shanghai) Co., Ltd.
 * All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are NOT permitted except as agreed by
 * iAUTO(Shanghai) Co., Ltd.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
#include <jni.h>
#include "vtservice.h"
#include "vtlog.h"
#include <memory>
//#ifdef __cplusplus
//extern "C" {
//#endif

static jclass jvtserverjni = nullptr;
static jmethodID jmethod_onNotifyEvent = nullptr;
static std::shared_ptr<vt::VTService> gService = nullptr;
static std::shared_ptr<vt::IVTServerListener> gservicejni = nullptr;
static JavaVM* gJvm = nullptr;

static JNIEnv* GetJNIEnv(bool &multi_thread) {
    LOGI("GetJNIEnv in");
    JNIEnv* env = nullptr;
    jint res = gJvm->GetEnv((void**)&env, JNI_VERSION_1_6);
    if (JNI_EDETACHED == res) {
        LOGI("AttachCurrentThread before");
        res = gJvm->AttachCurrentThread(reinterpret_cast<JNIEnv **>(&env), nullptr);
        LOGI("AttachCurrentThread after");
        if (JNI_OK == res) {
            multi_thread = true;
            LOGI("AttachCurrentThread success");
        } else {
            LOGE("AttachCurrentThread failed");
            return nullptr;
        }
    } else if (JNI_OK == res) {
        LOGI("has already AttachCurrentThread");
    } else {
        LOGE("JNI_EVERSION, specified version is not supported");
        return nullptr;
    }
    LOGI("GetJNIEnv out");
    return env;
}

static std::string jstring2stdstring(JNIEnv *env, jstring jstr) {
    std::string stdstring;
    do {
        if (nullptr == env || nullptr == jstr) {
            LOGE("env or jstr is null");
            break;
        }
        const char* cstr = env->GetStringUTFChars(jstr, JNI_FALSE);
        if (nullptr == cstr) {
            LOGE("cstr is null");
            break;
        }
        stdstring = std::string(cstr);
        env->ReleaseStringUTFChars(jstr, cstr);
    } while(0);
    return stdstring;
}

class VTServerjni : public vt::IVTServerListener {
public:
    VTServerjni() {
        LOGD("VTServerjni: Constructor");
    }
    virtual void NotifyEvent(int eventtype, int code, const char* msg) {
        LOGD("VTServerjni::NotifyEvent eventtype = %d, code = %d, msg = %s",  eventtype, code, msg);
        bool multiThread = false;
        JNIEnv* env = GetJNIEnv(multiThread);
        if (nullptr == env) {
            LOGE("env is null");
        } else {
            jint jtype = (jint)eventtype;
            jint jcode = (jint)code;
            jstring jmsg = env->NewStringUTF(msg);
            env->CallStaticVoidMethod(jvtserverjni, jmethod_onNotifyEvent, jtype, jcode, jmsg);
            if (multiThread) {
                gJvm->DetachCurrentThread();
            }
        }
    }
};

static jint JNI_NativeInitialize
        (JNIEnv *env, jobject thiz) {
    LOGD("JNI_NativeInitialize");
    if (env == nullptr) {
        LOGE("env is nullptr");
        return 1;
    }
    //get gJvm
    env->GetJavaVM(&gJvm);

    //VTServerBQJni
    jvtserverjni = static_cast<jclass>(env->NewGlobalRef(
            static_cast<jobject>(env->FindClass("com/iauto/vtserver/VTServerBQJni"))));
    if (jvtserverjni == nullptr) {
        LOGE("jvtserverjni is null");
        return 1;
    }
    if (jmethod_onNotifyEvent == nullptr) {
        jmethod_onNotifyEvent = env->GetStaticMethodID(jvtserverjni, "onNotifyEvent", "(IILjava/lang/String;)V");
    }
    if (jmethod_onNotifyEvent == nullptr) {
        LOGE("jvtserverjni method is null");
        return 1;
    }
    if (gService == nullptr) {
        gService = std::make_shared<vt::VTService>();
    } else {
        LOGI("gService is exit");
    }
    if (gservicejni == nullptr) {
        gservicejni = std::make_shared<VTServerjni>();
    } else {
        LOGI("gservicejni is exit");
    }
    gService->SetJniCallBack(gservicejni);
    return gService->Initialize();
}

static void JNI_NativeUninitialize
        (JNIEnv *env, jobject thiz) {
    LOGD("JNI_NativeUninitialize");
    if (env == nullptr) {
        LOGE("env is nullptr");
        return;
    }
    gService->Uninitialize();
}

static void JNI_NativeSetVideoDescription
        (JNIEnv *env, jobject thiz, jobject jdescription) {
    LOGD("JNI_NativeSetVideoDescription");
    if (env == nullptr) {
        LOGE("env is nullptr");
        return;
    }
    if (jdescription == nullptr) {
        LOGE("jdescription is nullptr");
        return;
    }
    jclass jclass_description = env->GetObjectClass(jdescription);
    if (jclass_description == nullptr) {
        LOGE("jclass_description is nullptr");
        return;
    }
    jfieldID jfield_height = env->GetFieldID(jclass_description, "height", "I");
    jfieldID jfield_width = env->GetFieldID(jclass_description, "width", "I");
    jfieldID jfield_videoFormat = env->GetFieldID(jclass_description, "videoFormat", "I");
    if (jfield_height == nullptr || jfield_width == nullptr || jfield_videoFormat == nullptr) {
        LOGE("jclass_description field is nullptr");
        return;
    }
    jint jheight = env->GetIntField(jdescription, jfield_height);
    jint jwidth = env->GetIntField(jdescription, jfield_width);
    jint jvideoFormat = env->GetIntField(jdescription, jfield_videoFormat);
    vt::VTJDescription description;
    description.SetHeight(jheight);
    description.SetWidth(jwidth);
    description.SetVideoFormat(jvideoFormat);
    gService->SetVideoDescription(description);
}

static jint JNI_NativeStart
        (JNIEnv *env, jobject thiz) {
    LOGD("JNI_NativeStart");
    if (env == nullptr) {
        LOGE("env is nullptr");
        return 1;
    }
    return gService->Start();
}

static jint JNI_NativeStop
        (JNIEnv *env, jobject thiz) {
    LOGD("JNI_NativeStop");
    if (env == nullptr) {
        LOGE("env is nullptr");
        return 1;
    }
    return gService->Stop();
}

static void JNI_NativeNotifyVideoData
        (JNIEnv *env, jobject thiz, jbyteArray jvideoData) {
    LOGD("JNI_NativeNotifyVideoData");
    if (env == nullptr) {
        LOGE("env is nullptr");
        return;
    }
    if (jvideoData == nullptr) {
        LOGE("jvideoData is nullptr");
        return;
    }
    jbyte *tmp = env->GetByteArrayElements(jvideoData, JNI_FALSE);
    jint dataSize = env->GetArrayLength(jvideoData);
    gService->NotifyVideoData(reinterpret_cast<unsigned char*>(tmp), dataSize);
    env->ReleaseByteArrayElements(jvideoData, tmp, JNI_FALSE);
    return;
}

static void JNI_NativeNotifyError
        (JNIEnv *env, jobject thiz, jint jcode, jstring jerrMsg) {
    LOGD("JNI_NativeNotifyError");
    if (env == nullptr) {
        LOGE("env is nullptr");
        return;
    }
    if (jerrMsg == nullptr) {
        LOGE("jerrMsg is nullptr");
        return;
    }
    std::string errMsg = jstring2stdstring(env, jerrMsg);
    gService->NotifyError(jcode, errMsg);
    return;
}

static JNINativeMethod sMethods[] = {
        {
                "nativeInitialize",
                "()I",
                (void*)JNI_NativeInitialize
        }, {
                "nativeUninitialize",
                "()V",
                (void*)JNI_NativeUninitialize
        }, {
                "nativeSetVideoDescription",
                "(Lcom/iauto/vtserver/VTDescription;)V",
                (void*)JNI_NativeSetVideoDescription
        }, {
                "nativeStart",
                "()I",
                (void*)JNI_NativeStart
        }, {
                "nativeStop",
                "()I",
                (void*)JNI_NativeStop
        }, {
                "nativeNotifyVideoData",
                "([B)V",
                (void*)JNI_NativeNotifyVideoData
        }, {
                "nativeNotifyError",
                "(ILjava/lang/String;)V",
                (void*)JNI_NativeNotifyError
        }
};

static int registerNativeMethods(JNIEnv* env, const char* className,
                                 JNINativeMethod* gMethods, int numMethods) {
    jclass clazz = env->FindClass(className);
    if (clazz == nullptr) {
        LOGE("can not find class: %s", className);
        return JNI_FALSE;
    }

    if (env->RegisterNatives(clazz, gMethods, numMethods) < 0) {
        LOGE("register natives error");
        return JNI_FALSE;
    }

    return JNI_TRUE;
}

// This function only registers the native methods
static int register_VTServerBQJni(JNIEnv *env) {
    return registerNativeMethods(env,"com/iauto/vtserver/VTServerBQJni", sMethods, sizeof(sMethods)/ sizeof(sMethods[0]));
}

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM* vm, void* reserved) {
    LOGI("vtserver : loading JNI");
    JNIEnv* env = nullptr;

    if (vm->GetEnv((void**) &env, JNI_VERSION_1_6) != JNI_OK) {
        LOGE("JNI version mismatch error");
        return JNI_ERR;
    }

    int status = register_VTServerBQJni(env);
    if (status == JNI_FALSE) {
        LOGE("jni adapter service registration failure, status: %d", status);
        return JNI_ERR;
    }

    LOGI("vtserver : loading JNI success");
    /* success -- return valid version number */
    return JNI_VERSION_1_6;
}

//#ifdef __cplusplus
//}
//#endif
