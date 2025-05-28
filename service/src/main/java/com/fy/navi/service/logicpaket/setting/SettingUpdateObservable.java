package com.fy.navi.service.logicpaket.setting;


import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Hashtable;
import java.util.Locale;

public final class SettingUpdateObservable {

    private final Hashtable<String, SettingUpdateObserver> mObservers;

    public static SettingUpdateObservable getInstance() {
        return SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final SettingUpdateObservable INSTANCE = new SettingUpdateObservable();
    }

    private SettingUpdateObservable() {
        mObservers = new Hashtable<>();
    }


    /**
     * 添加监听
     * @param key 唯一标识
     * @param observer 监听
     */
    public void addObserver(final String key, final SettingUpdateObserver observer) {
        if (!mObservers.contains(observer)) {
            mObservers.put(key, observer);
        }
    }

    /**
     * 移除监听
     * @param key 唯一标识
     * @param observer 监听
     */
    public void removeObserver(final String key, final SettingUpdateObserver observer) {
        mObservers.remove(key,observer);
    }

    /**
     * 通知监听
     * @param key 唯一标识
     * @param value 值
     */
    public void notifySettingChanged(final String key, final boolean value) {
        for (SettingUpdateObserver observer : mObservers.values()) {
            observer.onUpdateSetting(key, value);
        }
    }

    /**
     * 通知车牌变更
     * @param plateNumber 车牌号
     */
    public void setPlateNumber(final String plateNumber) {
        for (SettingUpdateObserver listener : mObservers.values()) {
            listener.onPlateNumberChanged(plateNumber);
        }
    }

    /**
     * 通知同步时间
     */
    public void onUpdateSyncTime() {
        final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-M-d HH:mm:ss", Locale.getDefault());
        final String formattedDate = dateFormat.format(new Date());
        for (SettingUpdateObserver listener : mObservers.values()) {
            listener.onUpdateSyncTime(formattedDate);
        }
    }

    /**
     * 重命名
     * @param name
     */
    public void onUpdateRename(final String name) {
        for (SettingUpdateObserver listener : mObservers.values()) {
            listener.onUpdateName(name);
        }
    }


    public interface SettingUpdateObserver{
        /**
         * 通知监听
         * @param key 唯一标识
         * @param value 值
         */
       default void onUpdateSetting(String key, boolean value){

       }

        /**
         * 通知车牌变更
         * @param plateNumber 车牌号
         */
       default void onPlateNumberChanged(String plateNumber){

       }

        /**
         * 通知同步时间
         * @param syncTime 同步时间
         */
       default void onUpdateSyncTime(String syncTime){

       }

        /**
         * 重命名
         * @param name
         */
       default void onUpdateName(String name) {

       }
    }
}
