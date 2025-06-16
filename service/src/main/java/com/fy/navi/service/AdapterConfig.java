package com.fy.navi.service;

import android.content.Context;

import com.android.utils.log.Logger;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class AdapterConfig {

    private static final String TAG = MapDefaultFinalTag.SERVICE_TAG;

    private static String getSdk() {
        return BuildConfig.MAP_SDK;
    }

    /**
     * Get Java object instance.
     *
     * @param pakateName tClass PackageName
     * @param className  tClass SimpleName
     */
    public static Object getObject(String pakateName, String className) {
        Logger.i(TAG, "getObject");
        String objName = pakateName + "." + AdapterConfig.getSdk() + "." + className;
        Logger.i(TAG, objName);
        try {
            return Class.forName(objName).newInstance();
        } catch (IllegalAccessException | InstantiationException | ClassNotFoundException error) {
            error.printStackTrace();
        }
        return null;
    }

    /**
     * Get Java object instance.
     *
     * @param pakateName tClass PackageName
     * @param className  tClass SimpleName
     * @param tClass     this tClass
     * @param context    SurfaceView Context
     * @param <T>        tClass
     * @return tClass Instance
     */
    public static <T> T getObject(String pakateName, String className, Class<T> tClass, Context context) {
        Logger.i("AdapterConfig", "getObject");
        String objName = pakateName + "." + AdapterConfig.getSdk() + "." + className;
        Logger.i("AdapterConfig", objName);
        try {
            Constructor<?> constructor = tClass.getConstructor(Context.class);
            return (T) constructor.newInstance(context);
        } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException | InstantiationException e) {
            throw new RuntimeException(e);
        }
    }


    /**
     * methodCall.
     */
    public static void methodCall(String pacakgeName, String getinstance, String initFunction,
                                  Object... args) {
        try {
            Class<?> class1 = Class.forName(pacakgeName);
            Method method = class1.getMethod(getinstance);
            Object instance = method.invoke(new Object());
            Method method1 = class1.getMethod(initFunction);
            method1.invoke(instance);
        } catch (Exception error) {
            Logger.e(error.toString());
        }
    }

    /**
     * methodCall.
     */
    public static Object objectMethodCall(String pacakgeName, String getinstance, String initFunction,
                                          Object... args) {
        Object value = null;
        try {
            Class<?> class1 = Class.forName(pacakgeName);
            Method method = class1.getMethod(getinstance);
            Object instance = method.invoke(new Object());
            Method method1 = class1.getMethod(initFunction);
            value = method1.invoke(instance);
        } catch (Exception error) {
            Logger.e(error.toString());
        }
        return value;
    }

    /**
     * callMapMethod.
     */
    public static void callMapMethod(String pacakgeName, String getInstance, String function,
                                     Object... args) {
        try {
            Class<?> class1 = Class.forName(pacakgeName);
            Object object = class1.getMethod(getInstance).invoke(new Object());
            class1.getMethod(function, new Class[]{int.class}).invoke(object, args);
        } catch (Exception error) {
            Logger.e(error.toString());
        }
    }

    /**
     * callMapMethod.
     */
    public static Object getObjectFromJar(String pacakgeName, String className, String platformName) {
        String objName = pacakgeName + "." + platformName + "." + className;
        Logger.i("AdapterConfig", objName);
        try {
            return Class.forName(objName).newInstance();
        } catch (IllegalAccessException | InstantiationException | ClassNotFoundException error) {
            Logger.i("AdapterConfig", "error:" , error.toString());
        }
        return null;
    }
}
