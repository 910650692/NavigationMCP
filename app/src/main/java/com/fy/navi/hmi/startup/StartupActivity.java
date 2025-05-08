package com.fy.navi.hmi.startup;

import android.Manifest;
import android.content.Intent;
import android.os.Environment;
import android.provider.Settings;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.activity.ManualActivateFragment;
import com.fy.navi.hmi.activity.NetActivateFailedDialog;
import com.fy.navi.hmi.databinding.ActivityStartupBinding;
import com.fy.navi.hmi.permission.PermissionUtils;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseActivity;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class StartupActivity extends BaseActivity<ActivityStartupBinding, StartupViewModel> {

    private Animation mRotateAnim;
    private NetActivateFailedDialog mFailedDialog;

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.MAIN_SCREEN_MAIN_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_startup;
    }

    @Override
    public int onFragmentId() {
        return R.id.fragment_container;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mRotateAnim = AnimationUtils.loadAnimation(this, R.anim.rotate_animation);
        mRotateAnim.setDuration(2000);
        mRotateAnim.setRepeatCount(Animation.INFINITE);
        mRotateAnim.setInterpolator(new LinearInterpolator());
        mBinding.mainImg.setVisibility(View.VISIBLE);

        mFailedDialog = new NetActivateFailedDialog(this);
        mFailedDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, " 重试网络激活");
                mViewModel.netActivateRetry();
            }

            @Override
            public void onCancelClick() {
                Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "激活失败，手动退出应用");
                finish();
            }
        });
    }

    @Override
    protected void onDestroy() {
        if (mRotateAnim != null) {
            Logger.i("startup onDestroy");
            mRotateAnim.cancel();
            mRotateAnim = null;
        }
        if (mFailedDialog.isShowing()) {
            mFailedDialog.cancel();
        }
        super.onDestroy();
    }

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
                Logger.i("StartupActivity", "所有文件修改权限申请结果");
                if (Environment.isExternalStorageManager())
                    PermissionUtils.getInstance().onRequestPermissionsResult(android.Manifest.permission.MANAGE_EXTERNAL_STORAGE, 0);
                else
                    PermissionUtils.getInstance().onRequestPermissionsResult(Manifest.permission.MANAGE_EXTERNAL_STORAGE, -1);
                break;
            case PermissionUtils.REQUEST_PERMISSION_OVERLAY_CODE:
                Logger.i("StartupActivity", "悬浮窗申请结果");
                if (Settings.canDrawOverlays(this))
                    PermissionUtils.getInstance().onRequestPermissionsResult(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, 0);
                else
                    PermissionUtils.getInstance().onRequestPermissionsResult(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, -1);
                break;
        }
    }

    /**
     * 控制激活动画
     *
     * @param show 是否显示
     */
    public void showActivatingView(final boolean show) {
        if (show) {
            mBinding.mainImg.setVisibility(View.GONE);
            mBinding.activatingImg.setVisibility(View.VISIBLE);
            mBinding.activatingTv.setVisibility(View.VISIBLE);
            mBinding.activatingImg.startAnimation(mRotateAnim);
        } else {
            mBinding.mainImg.setVisibility(View.VISIBLE);
            mBinding.activatingImg.setVisibility(View.GONE);
            mBinding.activatingTv.setVisibility(View.GONE);
            if (mRotateAnim != null) {
                mRotateAnim.cancel(); // 停止并重置动画
                mRotateAnim.reset();
            }
        }
    }

    /**
     * 显示网络激活失败弹窗
     */
    public void showNetActivateFailedDialog() {
        if (ConvertUtils.isEmpty(mFailedDialog) || mFailedDialog.isShowing()) {
            Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "dialog null or showing");
            return;
        }
        mFailedDialog.show();
    }

    /**
     * 显示变更后的弹窗
     */
    public void showChangedDialog() {
        mFailedDialog.setConfirmText();
        mFailedDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "确认跳转手动激活");
                addFragment(new ManualActivateFragment(), null);
            }

            @Override
            public void onCancelClick() {
                Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "网络激活失败，手动退出应用");
                finish();
            }
        });
        mFailedDialog.show();
    }
}