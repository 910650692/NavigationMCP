package com.fy.navi.hmi.startup;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Environment;
import android.provider.Settings;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityStartupBinding;
import com.fy.navi.exportservice.ScreenRecorder;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseActivity;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class StartupActivity extends BaseActivity<ActivityStartupBinding, StartupViewModel> {

    @Override
    public void onCreateBefore() {
        mScreenId = MapTypeId.MAIN_SCREEN_MAIN_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_startup;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {}

    @Override
    public void onInitData() {
        Intent intent = getIntent();
        //外部应用打开地图时指定的响应界面
        if (null != intent) {
            int intentPage = intent.getIntExtra(INaviConstant.PAGE_EXTRA, INaviConstant.OpenIntentPage.NONE);
            String keyword = "";
            PoiInfoEntity endPoint = null;
            switch (intentPage) {
                case INaviConstant.OpenIntentPage.SEARCH_PAGE:
                    //搜索关键字
                    keyword = intent.getStringExtra(INaviConstant.SEARCH_KEYWORD_EXTRA);
                    break;
                case INaviConstant.OpenIntentPage.ROUTE_PAGE:
                    //算路终点
                    endPoint = intent.getParcelableExtra(INaviConstant.ROUTE_END_POI);
                    break;
                default:
                    break;
            }
            if (intentPage != INaviConstant.OpenIntentPage.NONE) {
                mViewModel.setExtraParams(intentPage, keyword, endPoint);
            }
            intent.putExtra(INaviConstant.PAGE_EXTRA, INaviConstant.OpenIntentPage.NONE);
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        PermissionUtils.getInstance().onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        switch (requestCode) {
            case PermissionUtils.REQUEST_PERMISSION_EXTERNAL_CODE:
                Logger.i("lvww", "所有文件修改权限申请结果");
                if (Environment.isExternalStorageManager())
                    PermissionUtils.getInstance().onRequestPermissionsResult(android.Manifest.permission.MANAGE_EXTERNAL_STORAGE, 0);
                else
                    PermissionUtils.getInstance().onRequestPermissionsResult(Manifest.permission.MANAGE_EXTERNAL_STORAGE, -1);
                break;
            case PermissionUtils.REQUEST_PERMISSION_OVERLAY_CODE:
                Logger.i("lvww", "悬浮窗申请结果");
                if (Settings.canDrawOverlays(this))
                    PermissionUtils.getInstance().onRequestPermissionsResult(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, 0);
                else
                    PermissionUtils.getInstance().onRequestPermissionsResult(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, -1);
                break;
            case PermissionUtils.REQUEST_PERMISSION_MEDIA_PROJECTION:
                Logger.i("lvww", "媒体投影权限申请结果：" + resultCode);
                if (resultCode == Activity.RESULT_OK) {
                    Intent intent = new Intent(this, ScreenRecorder.class);
                    intent.putExtra("code", resultCode);
                    intent.putExtra("data", data);
                    MyFsaService.getInstance().initHudService(this, intent);
                    PermissionUtils.getInstance().updateMediaProjection(true);
                    PermissionUtils.getInstance().onRequestPermissionsResult(Context.MEDIA_PROJECTION_SERVICE, 0);
                } else {
                    PermissionUtils.getInstance().updateMediaProjection(false);
                    PermissionUtils.getInstance().onRequestPermissionsResult(Context.MEDIA_PROJECTION_SERVICE, -1);
                }
                break;
        }
    }
}