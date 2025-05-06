package com.fy.navi.hmi.launcher;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityLauncherSmallCardBinding;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseActivity;

/**
 * Author: QiuYaWei
 * Date: 2025/2/13
 * Description: [车机launcher桌面上显示的卡片， 注意名称已和第三方约定完成，请不要改动名字和路径]
 */
public class MapLauncherSmallCardActivity extends BaseActivity<ActivityLauncherSmallCardBinding, LauncherSmallCardViewModel> {
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

    }

    @Override
    public void onInitData() {
        Logger.i(TAG, "onInitData");
    }

    public IBaseScreenMapView getMapView(){
        return mBinding.mapScreenView;
    }
}
