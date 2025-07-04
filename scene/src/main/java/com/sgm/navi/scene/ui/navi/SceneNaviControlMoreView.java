package com.sgm.navi.scene.ui.navi;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneNaviControlMoreViewBinding;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.navi.SceneNaviControlMoreImpl;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.view.SwipeView;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;

/**
 * 底部控制更多scene
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviControlMoreView extends NaviSceneBase<SceneNaviControlMoreViewBinding, SceneNaviControlMoreImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CONTROL_MORE;
    private ImersiveStatus mImersiveStatus;

    public SceneNaviControlMoreView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviControlMoreView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviControlMoreView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                    final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneNaviControlMoreViewBinding createViewBinding(final LayoutInflater inflater,
                                                            final ViewGroup viewGroup) {
        return SceneNaviControlMoreViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviControlMoreImpl initSceneImpl() {
        return new SceneNaviControlMoreImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviControl(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_CONTROL_MORE;
    }

    @Override
    protected void init() {
        super.init();
        initVehicleType();
    }

    @Override
    public void show() {
        super.show();
        refreshView();
        if (mScreenViewModel != null) {
            mScreenViewModel.initTimer();
        }
    }

    /**
     * 刷新页面
     */
    public void refreshView() {
        final boolean isFixedOverView = NaviPackage.getInstance().getFixedOverViewStatus();
        Logger.i(TAG, "isFixedOverView:", isFixedOverView);
        mViewBinding.svCarHead.setAlpha(isFixedOverView ? 0.5f : 1.0f);
        mViewBinding.svCarHead.setIsClickChangeColor(!isFixedOverView);
        if (mScreenViewModel != null) {
            updateCarModel();
            updateBroadcast();
            initVehicleType();
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mScreenViewModel != null) {
            mScreenViewModel.cancelTimer();
        }
    }

    @Override
    public void close() {
        super.close();
        if (mScreenViewModel != null) {
            mScreenViewModel.cancelTimer();
        }
    }

    /**
     * @param currentImersiveStatus status
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        if (currentImersiveStatus != mImersiveStatus) {
            mImersiveStatus = currentImersiveStatus;
        } else {
            return;
        }
        if (mScreenViewModel != null) {
            mScreenViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }

    /**
     * 更新广播模式
     */
    public void updateBroadcast() {
        updateBroadcast(mScreenViewModel.getBroadcastMode());
    }

    public void updateCarModel() {
        updateCarModel(mScreenViewModel.getCarModel());
    }

    /**
     * @param type type
     */
    public void updateBroadcast(@NaviConstant.BroadcastType final int type) {
        switch (type) {
            case NaviConstant.BroadcastType.BROADCAST_CONCISE:
                mViewBinding.stvBroadcast.setText(R.string.navi_broadcast_concise);
                mViewBinding.sivBroadcast.setBackgroundResource(R.drawable.img_broadcast_succinct_58);
                break;
            case NaviConstant.BroadcastType.BROADCAST_DETAIL:
                mViewBinding.stvBroadcast.setText(R.string.navi_broadcast_detail);
                mViewBinding.sivBroadcast.setBackgroundResource(R.drawable.img_broadcast_detailed_58);
                break;
            case NaviConstant.BroadcastType.BROADCAST_MINIMALISM:
                mViewBinding.stvBroadcast.setText(R.string.navi_broadcast_minimalism);
                mViewBinding.sivBroadcast.setBackgroundResource(R.drawable.img_broadcast_brief_58);
                break;
            default:
                break;

        }
    }

    /**
     * @param mode 车头朝向类型
     */
    public String updateCarModel(final MapMode mode) {
        String carModel = "";
        switch (mode) {
            case NORTH_2D:
                carModel = getContext().getString(R.string.navi_north_2d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_2db_58);
                break;
            case UP_2D:
                carModel = getContext().getString(R.string.navi_up_2d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_2d_58);
                break;
            case UP_3D:
                carModel = getContext().getString(R.string.navi_up_3d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_3d_58);
                break;
            default:
                break;
        }
        mViewBinding.stvCarHead.setText(carModel);
        return carModel;
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    private void initVehicleType() {
        int vehicleType = mScreenViewModel.getCarType();
        // 纯油
        if (vehicleType == 0) {
            mViewBinding.sivCharge.setBackground(getContext().getDrawable
                    (R.drawable.img_lavatory_58));
            mViewBinding.stvCharge.setText(R.string.st_quick_search_lavatory);
            mViewBinding.sivLavatory.setBackground(getContext().getDrawable(
                    R.drawable.img_end_point_58));
            mViewBinding.stvLavatory.setText(R.string.navi_along_parking);
            mViewBinding.sivService.setBackground(getContext().getDrawable(
                    R.drawable.img_service_area_rim_58));
            mViewBinding.stvService.setText(R.string.navi_along_service);
            // 纯电
        } else if (vehicleType == 1) {
            mViewBinding.sivOil.setBackground(getContext().getDrawable(
                    R.drawable.img_lightning_58));
            mViewBinding.stvOil.setText(R.string.st_quick_search_charge);
            mViewBinding.sivCharge.setBackground(getContext().getDrawable
                    (R.drawable.img_lavatory_58));
            mViewBinding.stvCharge.setText(R.string.st_quick_search_lavatory);
            mViewBinding.sivLavatory.setBackground(getContext().getDrawable(
                    R.drawable.img_end_point_58));
            mViewBinding.stvLavatory.setText(R.string.navi_along_parking);
            mViewBinding.sivService.setBackground(getContext().getDrawable(
                    R.drawable.img_service_area_rim_58));
            mViewBinding.stvService.setText(R.string.navi_along_service);
        }
        // 默认写的是混动
    }

    /**
     * @param isConnected isConnected
     */
    public void onNetStatusChange(boolean isConnected) {
        Logger.i(TAG, "onNetStatusChange isConnected:", isConnected);
        mScreenViewModel.refreshRouteCauseNet();
    }

    public void setSwipeListener(SwipeView.DownSwipeAndClickListener downSwipeListener) {
        if (mViewBinding != null && mViewBinding.svBarArea != null) {
            if (mViewBinding.svBarArea instanceof SwipeView) {
                ((SwipeView) mViewBinding.svBarArea).setDownSwipeListener(downSwipeListener);
            }
        }
    }

    public void removeSwipeListener() {
        if (mViewBinding != null && mViewBinding.svBarArea != null) {
            if (mViewBinding.svBarArea instanceof SwipeView) {
                ((SwipeView) mViewBinding.svBarArea).setDownSwipeListener(null);
            }
        }
    }

    public void onPassByClick() {
        if (mScreenViewModel != null) {
            mScreenViewModel.alongSearch(NumberUtils.NUM_4);
        }
    }

    public void switchBroadcastMode(int broadcastMode) {
        if (mScreenViewModel != null) {
            mScreenViewModel.switchBroadcastMode(broadcastMode);
        }
    }

    public void updateSceneVisible(boolean b) {
        if (mScreenViewModel != null) {
            mScreenViewModel.notifySceneStateChange(b);
            if (mISceneCallback != null) {
                mISceneCallback.skipNaviControlScene();
            }
        }
    }
}
