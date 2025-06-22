package com.sgm.navi.hmi.launcher;

import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ActivityLauncherDeskBinding;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.ui.base.BaseActivity;

/**
 * Author: QiuYaWei
 * Date: 2025/2/13
 * Description: [车机launcher桌面上显示的主图]
 */
public class MapLauncherDeskActivity extends BaseActivity<ActivityLauncherDeskBinding, LauncherDeskViewModel>{
    private static final String TAG = "MapLauncherDeskActivity";

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.LAUNCHER_DESK_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_launcher_desk;
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

    }

    public IBaseScreenMapView getMapView() {
        return mBinding.mainMapview;
    }
}
