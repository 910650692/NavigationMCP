package com.fy.navi.mapservice.utils;

import android.os.MemoryFile;
import android.os.ParcelFileDescriptor;
import android.util.Log;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class PFDUtil {
    public static final String TAG = PFDUtil.class.getSimpleName();

    private static Object getFileDescriptor(MemoryFile memoryFile) {
        return ReflectUtil.invoke(
                "android.os.MemoryFile",
                memoryFile,
                "getFileDescriptor"
        );
    }

    public static ParcelFileDescriptor string2PFD(String interfaceName, String string) {
        return bytes2PFD(interfaceName, string.getBytes(StandardCharsets.UTF_8));
    }

    public static ParcelFileDescriptor bytes2PFD(String interfaceName, byte[] bytes) {
        try {
            MemoryFile memoryFile = new MemoryFile(interfaceName, bytes.length);
            memoryFile.writeBytes(bytes, 0, 0, bytes.length);
            Object fileDescriptor = getFileDescriptor(memoryFile);
            return ParcelFileDescriptor.dup((FileDescriptor) fileDescriptor);
        } catch (IOException e) {
            Log.e(TAG, "bytes2PFD: " + e);
            return null;
        }
    }

    public static String PFD2string(ParcelFileDescriptor pfd) {
        if (pfd == null) {
            Log.e(TAG, "PFD2string: pfd null");
            return "";
        }
        try {
            FileDescriptor fileDescriptor = pfd.getFileDescriptor();
            FileInputStream fileInputStream = new FileInputStream(fileDescriptor);
            byte[] bytes = fileInputStream.readAllBytes();
            return new String(bytes, StandardCharsets.UTF_8);
        } catch (IOException e) {
            Log.e(TAG, "PFD2string error: " + e);
            return "";
        }
    }
}
