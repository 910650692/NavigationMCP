package com.fy.navi.hmi.launcher;

import android.os.Bundle;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.INaviInitListener;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityLauncherDeskBinding;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseActivity;

/**
 * Author: QiuYaWei
 * Date: 2025/2/13
 * Description: [车机launcher桌面上显示的主图]
 */
public class MapLauncherDeskActivity extends BaseActivity<ActivityLauncherDeskBinding, BaseLauncherDeskViewModel> implements INaviInitListener {
    private static final String TAG = "MapLauncherDeskActivity";

    @Override
    public int onLayoutId() {
        return R.layout.activity_launcher_desk;
    }

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.LAUNCHER_DESK_MAP.name();
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (!NaviService.isMapInited) {
            LauncherManager.getInstance().startInitService();
            NaviService.registerAppInitListener(this);
        }
    }

    @Override
    protected void onStart() {
        super.onStart();
        Logger.d(TAG, "onStart");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        NaviService.unRegisterAppInitListener(this);
        Logger.d(TAG, "onDestroy");
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        if (NaviService.isMapInited) {
            mViewModel.loadMapView();
        }
    }

    @Override
    public void onInitData() {

    }

    @Override
    protected void onMoveMapCenter() {
    }

    @Override
    protected void onResetMapCenter() {
    }

    @Override
    public void onInitFinished(boolean isSuccess) {
        Logger.i(TAG, "onInitFinished:" + isSuccess);
        if (isSuccess) {
            mViewModel.loadMapView();
        }
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.mainMapview;
    }

    /*private void showFloatingWindow() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M && !Settings.canDrawOverlays(this)) {
            val intent = Intent(
                    Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
                    Uri.parse("package:$packageName")
            )
            startActivityForResult(intent, REQUEST_CODE_DRAW_OVERLAY_PERMISSION)
        } else {
            startFloatingWindowService()
        }
    }*/
}
