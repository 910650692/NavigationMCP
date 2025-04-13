package com.iauto.vtserver;

import android.util.Log;

public class VTServerBQJni {
    private static final VTServerBQJni MJNI = new VTServerBQJni();
    private static IVTServerListener mListener = null;
    private static boolean isSuccessLoadLibrary = false;
    static {
        try {
            Log.d("vtservice-jni", "VTServerBQJni static loadLibrary");
            System.loadLibrary("vtservice-jni");
            isSuccessLoadLibrary = true;
        } catch (Throwable e) {
            Log.e("vtservice-jni", "static initializer: " + e.getMessage());
        }
    }
    private VTServerBQJni() {
    }
    public static VTServerBQJni getInstance() {
        return MJNI;
    }

    // APP Reply function used by C++
    public static void onNotifyEvent(int eventtype, int code, String msg) {
        Log.d("vtservice-jni", "onNotifyEvent eventtype = " + eventtype + ", code = " + code + ", msg = " + msg);
        if (mListener != null) {
            mListener.onNotifyEvent(eventtype, code, msg);
        }
    }
    //set callback
    public void setVTServerListener(IVTServerListener listener) {
        Log.d("vtservice-jni", "setVTServerListener");
        mListener = listener;
    }

    public boolean isIsSuccessLoadLibrary() {
        return isSuccessLoadLibrary;
    }

    public native int nativeInitialize();

    public native void nativeUninitialize();

    public native void nativeSetVideoDescription(VTDescription description);

    public native int nativeStart();

    public native int nativeStop();

    public native void nativeNotifyVideoData(byte[] videoData);

    public native void nativeNotifyError(int code, String errMsg);
}
