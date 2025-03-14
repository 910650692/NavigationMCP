package com.fy.navi.hmi.setting;

import com.fy.navi.ui.base.BaseModel;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/11
 */
public class SettingModel extends BaseModel<SettingViewModel> {

    public SettingModel() {
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 判断网络是否在线
     * @return
     */
    public boolean isNetOffline() {
        // TODO: 2025/1/15
        return false;
    }

    /**
     * 是否同步收藏夹
     * @param isFavorites
     */
    public void onSyncData(boolean isFavorites) {
        if (isFavorites) {
            return;
        }
        // TODO: 2025/1/15
    }
}
