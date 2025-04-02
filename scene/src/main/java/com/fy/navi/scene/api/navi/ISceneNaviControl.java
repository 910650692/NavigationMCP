package com.fy.navi.scene.api.navi;


import androidx.databinding.ObservableField;

import com.fy.navi.ui.view.SkinTextView;

public interface ISceneNaviControl {
    //关闭导航
    void closeNavi();

    //继续导航
    void naviContinue();

    //全览切换
    void switchOverview();

    //静音\固定全览
    void onVariation();

    //更多设置
    void moreSetup();

    //刷新
    void refreshRoute();

    //导航播报
    void naviBroadcast();

    //路线偏好
    void routePreference();

    //车头朝上
    void carHead();

    //导航设置
    void naviSetting();

    //沿途搜索
    void alongSearch(int index);

    /**
     * 显示主界面
     */
    void showMain();

    ObservableField<Boolean> getControlField();

    ObservableField<Boolean> getGroupOneField();

    ObservableField<Boolean> getGroupTwoField();

    ObservableField<Boolean> getGroupMoreSetupField();
}
