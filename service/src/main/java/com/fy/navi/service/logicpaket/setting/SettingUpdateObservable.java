package com.fy.navi.service.logicpaket.setting;

import com.android.utils.log.Logger;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Hashtable;
import java.util.Locale;

public class SettingUpdateObservable {

    private final Hashtable<String, SettingUpdateObserver> observers;
    private String currentPlateNumber = "";

    public static SettingUpdateObservable getInstance() {
        return SInstanceHolder.sInstance;
    }

    private static final class SInstanceHolder {
        static final SettingUpdateObservable sInstance = new SettingUpdateObservable();
    }

    private SettingUpdateObservable() {
        observers = new Hashtable<>();
    }


    public void addObserver(String key, SettingUpdateObserver observer) {
        if (!observers.contains(observer)) {
            observers.put(key, observer);
        }
    }

    public void removeObserver(String key, SettingUpdateObserver observer) {
        observers.remove(key,observer);
    }

    public void notifySettingChanged(String key, boolean value) {
        for (SettingUpdateObserver observer : observers.values()) {
            observer.onUpdateSetting(key, value);
        }
    }

    public void setPlateNumber(String plateNumber) {
        this.currentPlateNumber = plateNumber;
        for (SettingUpdateObserver listener : observers.values()) {
            listener.onPlateNumberChanged(plateNumber);
        }
    }

    public void onUpdateSyncTime() {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-M-d HH:mm:ss", Locale.getDefault());
        String formattedDate = dateFormat.format(new Date());
        for (SettingUpdateObserver listener : observers.values()) {
            listener.onUpdateSyncTime(formattedDate);
        }
    }


    public interface SettingUpdateObserver{
       default void onUpdateSetting(String key, boolean value){}
       default void onPlateNumberChanged(String plateNumber){}
       default void onUpdateSyncTime(String syncTime){}
    }
}
