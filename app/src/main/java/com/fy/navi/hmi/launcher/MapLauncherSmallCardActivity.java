package com.fy.navi.hmi.launcher;

import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityLauncherSmallCardBinding;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseActivity;

/**
 * Author: QiuYaWei
 * Date: 2025/2/13
 * Description: [车机launcher桌面上显示的卡片， 注意名称已和第三方约定完成，请不要改动名字和路径]
 */
public class MapLauncherSmallCardActivity extends BaseActivity<ActivityLauncherSmallCardBinding, BaseLauncherSmallCardViewModel> {
    private static final String TAG = "MapLauncherSmallCardActivity";

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.LAUNCHER_WIDGET_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_launcher_small_card;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        switchUI(false);
    }

    @Override
    public void onInitData() {
        Logger.i(TAG, "onInitData");
    }

    /***
     * 切换UI
     */
    public void switchUI(final boolean isAllReady) {
        Logger.i(TAG, "switchUI", "isAllReady:" + isAllReady);
        final FragmentManager fragmentManager = getSupportFragmentManager();
        final Fragment willShow = isAllReady ? new SmallCardMapFragment() : new SmallCardPlaceHolderFragment();
        fragmentManager.beginTransaction().replace(R.id.fragment_container, willShow).commit();
    }
}
