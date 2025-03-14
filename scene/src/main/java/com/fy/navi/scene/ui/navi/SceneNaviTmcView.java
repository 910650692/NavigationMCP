package com.fy.navi.scene.ui.navi;


import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_TMC;

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviTmcViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviTmcImpl;
import com.fy.navi.scene.impl.navi.common.SceneCommonStruct;
import com.fy.navi.scene.impl.navi.common.SceneEnumRes;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.scene.impl.navi.common.NaviUiUtil;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;

/**
 * 路况条scene
 */
public class SceneNaviTmcView extends NaviSceneBase<SceneNaviTmcViewBinding, SceneNaviTmcImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private List<NaviTmcInfo.NaviTmcInfoData> tmcBarItemsNew;
    // 途径点信息
    private ArrayList<NaviEtaInfo.NaviTimeAndDist> mViaRemain;
    // 上一次显示的途径点信息
    private ArrayList<NaviEtaInfo.NaviTimeAndDist> mLastViaRemain = new ArrayList<>();
    // 充电站信息
    private ArrayList<NaviEtaInfo.NaviTimeAndDist> mChargeStationRemain;
    // 上一次显示的途径点信息
    private ArrayList<NaviEtaInfo.NaviTimeAndDist> mLastChargeStationRemain = new ArrayList<>();
    // 途径点显示索引
    private static int mViaShowIndex = 0;
    // 途经点和充电站最多显示20个
    private final static int MAX_VIA_NUM = 20;
    /**
     * < tmcBar累积的总距离，注意与当前路线长度不同，重算后_totalDistance不是新路线的长度，
     * 而是_distanceHasPassed加上新路线长度，避免重算后tmcBar回到起点
     */
    private long mTotalDistance;
    /**
     * tmcBar累积的已走距离,不会清零
     */
    private long mDistanceHasPassed;
    private boolean isHorizontal = true;
    private boolean mOffline;
    private int viaWidth;

    public SceneNaviTmcView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviTmcView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviTmcView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        viaWidth = ScreenUtils.Companion.getInstance().dp2px(10);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_TMC;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_TMC, this);
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_TMC, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_TMC, true);
        }
    }

    @Override
    protected SceneNaviTmcViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviTmcViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviTmcImpl initSceneImpl() {
        return new SceneNaviTmcImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {
        mScreenViewModel.registerObserver();
    }

    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onUpdateTMCLightBar(naviTmcInfo);
        }
    }

    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviETAInfo);
        }
    }

    @Override
    public void onDestroy() {
        mScreenViewModel.unregisterObserver();
        super.onDestroy();
    }

    /**
     * 初始化
     */
    public void initTmcContainer(boolean offline) {
        setOffline(offline);
        mViewBinding.tmrtrResources.initTmcContainer(offline, mViewBinding);
    }

    /**
     * 更新光柱图数据view
     */
    public void updateTmcContainerNew(List<NaviTmcInfo.NaviTmcInfoData> tbitem, long distanceHasPassed, long totalDistance) {
//        Logger.d(TAG, "updateTmcContainerNew");
        setTmcContainerDataNew(tbitem, distanceHasPassed, totalDistance);
        invalidate();
    }

    public void setTmcContainerDataNew(List<NaviTmcInfo.NaviTmcInfoData> items, long distanceHasPassed, long totalDistance) {
        tmcBarItemsNew = items;
        mDistanceHasPassed = distanceHasPassed;
        mTotalDistance = totalDistance;
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {
        super.dispatchDraw(canvas);
//        Logger.d(TAG, "dispatchDraw");
        drawTmcContainer(canvas);
    }

    /**
     * 绘制光柱图
     *
     * @param canvas
     */
    private void drawTmcContainer(Canvas canvas) {
        // 没有光柱图信息时不显示途径点几充电站信息
        // 重置途径点显示索引
        mViaShowIndex = 0;
        // 光主图高度
        int height = mViewBinding.tmrtrResources.getBottom() - mViewBinding.tmrtrResources.getTop() - mViewBinding.sivCar.getHeight() / 2;
        int width = mViewBinding.tmrtrResources.getRight() - mViewBinding.tmrtrResources.getLeft();
        // 计算总的路程
        long hasPassedDistance = 0;
        if (tmcBarItemsNew != null) {
            for (NaviTmcInfo.NaviTmcInfoData item : tmcBarItemsNew) {
                // 已走过的路程
                if (item.status == 10) {
                    hasPassedDistance = item.distance;
                }
            }
        }

        float rateDistanceToView;
        if (isHorizontal) {
            rateDistanceToView = (width * 1.0f) / (mTotalDistance * 1.0f);
        } else {
            //距离和View高度的比率,用于在view高度和实际距离之间进行转换,单位:像素/米
            rateDistanceToView = (height * 1.0f) / (mTotalDistance * 1.0f);
        }

        // 车标位置
        int carPosition;
        if (isHorizontal) {
            carPosition = Math.round((hasPassedDistance + mDistanceHasPassed) * rateDistanceToView);
        } else {
            carPosition = Math.round((mTotalDistance - hasPassedDistance - mDistanceHasPassed) * rateDistanceToView);
        }
        Logger.d(TAG, "drawTmcContainer mTotalDistance:" + mTotalDistance + " hasPassedDistance:" + hasPassedDistance + " mDistanceHasPassed :" +
                mDistanceHasPassed);
        Logger.d(TAG, "drawTmcContainer width:" + width + " rateDistanceToView:" + rateDistanceToView + " carPosition :" + carPosition);
        // 移动车标的Y坐标
        NaviUiUtil.setTranslation(mViewBinding.sivCar, carPosition, isHorizontal);
        // 绘制途径点
        if (mViaRemain != null && !mViaRemain.isEmpty()) {
            for (int i = 0; i < mViaRemain.size() && i < MAX_VIA_NUM; i++) {
                NaviEtaInfo.NaviTimeAndDist viaItem = mViaRemain.get(i);
                int via;
                if (isHorizontal) {
                    via = Math.min(Math.round((hasPassedDistance + mDistanceHasPassed + viaItem.dist) * rateDistanceToView), width - viaWidth);
                } else {
                    via = Math.round((mTotalDistance - hasPassedDistance - mDistanceHasPassed - viaItem.dist) * rateDistanceToView);
                }
                mViaShowIndex++;
                showViaIcon(i, SceneCommonStruct.TmcViaPointType.ViaPointType, via);
            }
        } else {
            // 不显示途经点
            for (int i = 0; i < mLastViaRemain.size() && i < MAX_VIA_NUM; i++) {
                hideViaIcon(i);
            }
        }
        // 绘制充电站
        if (mChargeStationRemain != null && !mChargeStationRemain.isEmpty()) {
            for (int i = 0; i < mChargeStationRemain.size() && i < MAX_VIA_NUM; i++) {
                NaviEtaInfo.NaviTimeAndDist viaItem = mChargeStationRemain.get(i);
                int via;
                if (isHorizontal) {
                    via = Math.round((hasPassedDistance + mDistanceHasPassed + viaItem.dist) * rateDistanceToView);
                } else {
                    via = Math.round((mTotalDistance - hasPassedDistance - mDistanceHasPassed - viaItem.dist) * rateDistanceToView);
                }
                Logger.d(TAG, "mChargeStationRemain size:" + mViaRemain.size() + "-----via:" + via);
                showViaIcon(mViaShowIndex + i, SceneCommonStruct.TmcViaPointType.ViaChargeType, via);
            }
        } else {
            // 不显示充电站
            int offsetIndex = mLastViaRemain == null ? 0 : mLastViaRemain.size();
            for (int i = 0; i < mLastChargeStationRemain.size() && i < MAX_VIA_NUM; i++) {
                hideViaIcon(offsetIndex + i);
            }
        }
    }

    /**
     * 设置途径点和充电桩数据
     *
     * @param viaRemain
     * @param chargeStationRemain
     */
    public void updateTmcVia(ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain, ArrayList<NaviEtaInfo.NaviTimeAndDist> chargeStationRemain) {
        // 保存上一次显示的途经点信息
        mLastViaRemain.clear();
        if (mViaRemain != null && !mViaRemain.isEmpty()) {
            mLastViaRemain.addAll(mViaRemain);
        }
        if (viaRemain != null && mViaRemain != null && viaRemain.size() != mViaRemain.size()) {
            resetView();
        }
        mViaRemain = viaRemain;

        // 保存上一次显示的充电站信息
        mLastChargeStationRemain.clear();
        if (mChargeStationRemain != null && !mChargeStationRemain.isEmpty()) {
            mLastChargeStationRemain.addAll(mLastChargeStationRemain);
        }
        if (chargeStationRemain != null && mChargeStationRemain != null && chargeStationRemain.size() != mChargeStationRemain.size()) {
            resetView();
        }
        mChargeStationRemain = chargeStationRemain;
    }

    public void resetView() {
        if (mViewBinding == null) {
            return;
        }
        NaviUiUtil.hideView(mViewBinding.siv1, mViewBinding.siv2, mViewBinding.siv31, mViewBinding.siv32, mViewBinding.siv33,
                mViewBinding.siv34, mViewBinding.siv35, mViewBinding.siv36, mViewBinding.siv37, mViewBinding.siv38,
                mViewBinding.siv39, mViewBinding.siv310, mViewBinding.siv311, mViewBinding.siv312, mViewBinding.siv313,
                mViewBinding.siv314, mViewBinding.siv315, mViewBinding.siv316, mViewBinding.siv317, mViewBinding.siv3);
    }

    private void showViaIcon(View view, SceneCommonStruct.TmcViaPointType tmcViaPointType, int viaY) {
        if (view instanceof SkinTextView stv) {
            if (tmcViaPointType == SceneCommonStruct.TmcViaPointType.ViaPointType) {
                stv.setText(getContext().getText(R.string.navi_via_item_pass));
            } else if (tmcViaPointType == SceneCommonStruct.TmcViaPointType.ViaChargeType) {
                stv.setText(getContext().getText(R.string.navi_via_item_charge));
            }
        }
        NaviUiUtil.showView(view);
        setViaBackground(view, tmcViaPointType);
        NaviUiUtil.setTranslation(view, viaY, isHorizontal);
    }

    /**
     * 显示途径点图标
     */
    private void showViaIcon(int index, SceneCommonStruct.TmcViaPointType tmcViaPointType, int viaY) {
        Logger.d(TAG, "showViaIcon size:" + mViaRemain.size() + ",index :" + index + ",tmcViaPointType：" + tmcViaPointType
                + ",viaY：" + viaY);
        switch (index) {
            case NaviConstant.TMCViaIndex.Via0:
                showViaIcon(mViewBinding.siv1, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via1:
                showViaIcon(mViewBinding.siv2, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via2:
                showViaIcon(mViewBinding.siv31, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via3:
                showViaIcon(mViewBinding.siv32, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via4:
                showViaIcon(mViewBinding.siv33, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via5:
                showViaIcon(mViewBinding.siv34, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via6:
                showViaIcon(mViewBinding.siv35, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via7:
                showViaIcon(mViewBinding.siv36, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via8:
                showViaIcon(mViewBinding.siv37, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via9:
                showViaIcon(mViewBinding.siv38, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via10:
                showViaIcon(mViewBinding.siv39, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via11:
                showViaIcon(mViewBinding.siv310, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via12:
                showViaIcon(mViewBinding.siv311, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via13:
                showViaIcon(mViewBinding.siv312, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via14:
                showViaIcon(mViewBinding.siv313, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via15:
                showViaIcon(mViewBinding.siv314, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via16:
                showViaIcon(mViewBinding.siv315, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via17:
                showViaIcon(mViewBinding.siv316, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via18:
                showViaIcon(mViewBinding.siv317, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.Via19:
                showViaIcon(mViewBinding.siv3, tmcViaPointType, viaY);
                break;
            default:
                break;
        }
    }

    /**
     * 隐藏途径点图标
     */
    private void hideViaIcon(int index) {
        Logger.d(TAG, "hideViaIcon index :" + index);
        switch (index) {
            case NaviConstant.TMCViaIndex.Via0:
                NaviUiUtil.hideView(mViewBinding.siv1);
                break;
            case NaviConstant.TMCViaIndex.Via1:
                NaviUiUtil.hideView(mViewBinding.siv2);
                break;
            case NaviConstant.TMCViaIndex.Via2:
                NaviUiUtil.hideView(mViewBinding.siv31);
                break;
            case NaviConstant.TMCViaIndex.Via3:
                NaviUiUtil.hideView(mViewBinding.siv32);
                break;
            case NaviConstant.TMCViaIndex.Via4:
                NaviUiUtil.hideView(mViewBinding.siv33);
                break;
            case NaviConstant.TMCViaIndex.Via5:
                NaviUiUtil.hideView(mViewBinding.siv34);
                break;
            case NaviConstant.TMCViaIndex.Via6:
                NaviUiUtil.hideView(mViewBinding.siv35);
                break;
            case NaviConstant.TMCViaIndex.Via7:
                NaviUiUtil.hideView(mViewBinding.siv36);
                break;
            case NaviConstant.TMCViaIndex.Via8:
                NaviUiUtil.hideView(mViewBinding.siv37);
                break;
            case NaviConstant.TMCViaIndex.Via9:
                NaviUiUtil.hideView(mViewBinding.siv38);
                break;
            case NaviConstant.TMCViaIndex.Via10:
                NaviUiUtil.hideView(mViewBinding.siv39);
                break;
            case NaviConstant.TMCViaIndex.Via11:
                NaviUiUtil.hideView(mViewBinding.siv310);
                break;
            case NaviConstant.TMCViaIndex.Via12:
                NaviUiUtil.hideView(mViewBinding.siv311);
                break;
            case NaviConstant.TMCViaIndex.Via13:
                NaviUiUtil.hideView(mViewBinding.siv312);
                break;
            case NaviConstant.TMCViaIndex.Via14:
                NaviUiUtil.hideView(mViewBinding.siv313);
                break;
            case NaviConstant.TMCViaIndex.Via15:
                NaviUiUtil.hideView(mViewBinding.siv314);
                break;
            case NaviConstant.TMCViaIndex.Via16:
                NaviUiUtil.hideView(mViewBinding.siv315);
                break;
            case NaviConstant.TMCViaIndex.Via17:
                NaviUiUtil.hideView(mViewBinding.siv316);
                break;
            case NaviConstant.TMCViaIndex.Via18:
                NaviUiUtil.hideView(mViewBinding.siv317);
                break;
            case NaviConstant.TMCViaIndex.Via19:
                NaviUiUtil.hideView(mViewBinding.siv3);
                break;
            default:
                break;
        }
    }

    public static void setViaBackground(View view, SceneCommonStruct.TmcViaPointType type) {
        view.setBackgroundResource(SceneEnumRes.getDrawableEnumName0(type).getDayDrawableId());
    }

    /**
     * 更新光柱图数据view
     */
    public void updateTmcAreaNew(List<NaviTmcInfo.NaviTmcInfoData> tbitem, long distanceHasPassed, long totalDistance) {
        mViewBinding.tmrtrResources.updateTmcAreaNew(tbitem, distanceHasPassed, totalDistance);
    }

    public void setOffline(boolean offline) {
        mOffline = offline;
        if (mOffline) {
//            mViewBinding.carBottomLine.setImageResource(R.color.auto_color_0a80fb);
        } else {
//            mViewBinding.carBottomLine.setImageResource(R.color.auto_color_ca6d);
        }
//        mViewBinding.tmrtrResources.setOffline(offline);
    }
}
