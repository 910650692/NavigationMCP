package com.fy.navi.hmi.launcher;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description TODO 功能待完善，代码可以参照主图
 * @Author yaWei
 * @date 2025/2/18
 */
public class BaseLauncherSmallCardViewModel extends BaseViewModel<MapLauncherSmallCardActivity, LauncherSmallCardModel> {
    private static final String TAG = "BaseLauncherSmallCardViewModel";

    public BaseLauncherSmallCardViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected LauncherSmallCardModel initModel() {
        return new LauncherSmallCardModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        checkAgreementIsAgree();
    }

    /***
     * 检查隐私协议是否已同意
     */
    private void checkAgreementIsAgree() {

    }

    public void notifyMapInit() {
        mView.switchUI(true);
    }
}
