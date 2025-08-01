package com.sgm.navi.scene.ui.navi;

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.BuildConfig;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneNaviTmcViewBinding;
import com.sgm.navi.scene.impl.navi.SceneNaviTmcImpl;
import com.sgm.navi.scene.impl.navi.common.NaviUiUtil;
import com.sgm.navi.scene.impl.navi.common.SceneCommonStruct;
import com.sgm.navi.scene.impl.navi.common.SceneEnumRes;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;

/**
 * 路况条scene
 *
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviTmcView extends NaviSceneBase<SceneNaviTmcViewBinding, SceneNaviTmcImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_TMC;
    private List<NaviTmcInfo.NaviTmcInfoData> mTmcBarItemsNew;
    // 途径点信息
    private ArrayList<NaviEtaInfo.NaviTimeAndDist> mViaRemain;
    // 充电站信息
    private ArrayList<NaviEtaInfo.NaviTimeAndDist> mChargeStationRemain;
    private boolean mIsShowAutoAddChargeStation = true;
    // 途经点和充电站最多显示20个
    private final static int MAX_VIA_NUM = 20;
    // 途经点和充电站显示宽度
    private final static int VIA_WIDTH = 55;
    // 充电站显示索引
    int mLastChargeShowIndex = 0;
    // 途经点显示索引
    int mLastViaShowIndex = 0;
    /**
     * < tmcBar累积的总距离，注意与当前路线长度不同，重算后_totalDistance不是新路线的长度，
     * 而是_distanceHasPassed加上新路线长度，避免重算后tmcBar回到起点
     */
    private long mTotalDistance;
    /**
     * tmcBar累积的已走距离,不会清零
     */
    private long mDistanceHasPassed;
    private boolean mIsHorizontal = true;
    private boolean mOffline;
    private int mViaWidth;
    //光柱图充电站扎点防抖缓存
    private long[] mViaChargeArr;
    //光柱图途经点扎点防抖缓存
    private long[] mViaRemainArr;
    //控制TMC刷新频率
    private int mInvalidateCount;
    //上次充电站数量
    private int mLastChargeSize;
    //上次途经点数量
    private int mLastViaSize;
    private int mTmcHeight;
    private int mTmcWidth;

    public SceneNaviTmcView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviTmcView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviTmcView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                            final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mViaWidth = ScreenUtils.Companion.getInstance().dp2px(10);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_TMC;
    }

    @Override
    protected SceneNaviTmcViewBinding createViewBinding(final LayoutInflater inflater,
                                                        final ViewGroup viewGroup) {
        return SceneNaviTmcViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviTmcImpl initSceneImpl() {
        return new SceneNaviTmcImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviTMC(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        mScreenViewModel.registerObserver();
    }

    /**
     * @param naviTmcInfo 路况信息
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onUpdateTMCLightBar(naviTmcInfo);
        }
    }

    /**
     * @param naviETAInfo 导航信息
     */
    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviETAInfo);
        }
    }

    public void onNaviInfoByViaArrived(NaviEtaInfo naviEtaInfo){
        hideViaIcon(0);
    }

    /**
     * 初始化
     *
     * @param offline 是否离线
     */
    public void initTmcContainer(final boolean offline) {
        setOffline(offline);
        mViewBinding.tmrtrResources.initTmcContainer(offline, mViewBinding);
    }

    /**
     * 更新光柱图数据view
     *
     * @param tbitem            item
     * @param distanceHasPassed 经过的总距离
     * @param totalDistance     总距离
     */
    public void updateTmcContainerNew(final List<NaviTmcInfo.NaviTmcInfoData> tbitem,
                                      final long distanceHasPassed, final long totalDistance) {
        if (mInvalidateCount > 2 || isViaChange(totalDistance)) {
            mInvalidateCount = 0;
        }
        setTmcContainerDataNew(tbitem, distanceHasPassed, totalDistance);
        if (mInvalidateCount == 0) {
            updateTmcAreaNew(tbitem, distanceHasPassed, totalDistance);
            invalidate();
        }
        mInvalidateCount++;
    }

    /**
     * @param items             items
     * @param distanceHasPassed 已经经过的距离
     * @param totalDistance     总距离
     */
    public void setTmcContainerDataNew(final List<NaviTmcInfo.NaviTmcInfoData> items,
                                       final long distanceHasPassed, final long totalDistance) {
        mTmcBarItemsNew = items;
        mDistanceHasPassed = distanceHasPassed;
        mTotalDistance = totalDistance;
    }

    @Override
    protected void dispatchDraw(final Canvas canvas) {
        super.dispatchDraw(canvas);
        drawTmcContainer(canvas);
    }

    private int getTmcHeight() {
        if (mTmcHeight == 0) {
            mTmcHeight = mViewBinding.tmrtrResources.getBottom() - mViewBinding.tmrtrResources.getTop() - mViewBinding.sivCar.getHeight() / 2;
        }
        return mTmcHeight;
    }

    private int getTmcWidth() {
        if (mTmcWidth == 0) {
            mTmcWidth = mViewBinding.tmrtrResources.getRight() - mViewBinding.tmrtrResources.getLeft();
        }
        return mTmcWidth;
    }

    /**
     * 绘制光柱图
     *
     * @param canvas canvas
     */
    private void drawTmcContainer(final Canvas canvas) {
        if (mTotalDistance <= 0) {
            Logger.i(TAG, " mTotalDistance <= 0, return");
            return;
        }
        // 光主图高度
        final int height = getTmcHeight();
        final int width = getTmcWidth();
        // 计算总的路程
        long hasPassedDistance = 0;
        if (mTmcBarItemsNew != null) {
            for (NaviTmcInfo.NaviTmcInfoData item : mTmcBarItemsNew) {
                // 已走过的路程
                if (item.getStatus() == 10) {
                    hasPassedDistance = item.getDistance();
                }
            }
        }

        final float rateDistanceToView;
        if (mIsHorizontal) {
            if (width <= 0) {
                Logger.i(TAG, "width <= 0, return");
                return;
            }
            rateDistanceToView = (width * 1.0f) / (mTotalDistance * 1.0f);
        } else {
            if (height <= 0) {
                Logger.i(TAG, "height <= 0, return");
                return;
            }
            //距离和View高度的比率,用于在view高度和实际距离之间进行转换,单位:像素/米
            rateDistanceToView = (height * 1.0f) / (mTotalDistance * 1.0f);
        }

        // 车标位置
        final int carPosition;
        if (mIsHorizontal) {
            carPosition = Math.round((hasPassedDistance + mDistanceHasPassed) * rateDistanceToView);
        } else {
            carPosition = Math.round((mTotalDistance - hasPassedDistance - mDistanceHasPassed) * rateDistanceToView);
        }
        if (Logger.openLog) {
            Logger.d(TAG, "mTotal:", mTotalDistance, " hasPassed:", hasPassedDistance, " mDistanceHas :", mDistanceHasPassed, " width:",
                    width, " rateDistanceToView:", rateDistanceToView, " carPosition :", carPosition, " mChargeStationRemain:",
                    ConvertUtils.isEmpty(mChargeStationRemain) ? "null" : mChargeStationRemain.size(), " mViaRemain:",
                    ConvertUtils.isEmpty(mViaRemain) ? "null" : mViaRemain.size(), " AutoAdd:", mIsShowAutoAddChargeStation,
                    " mLastChargeShowIndex:", mLastChargeShowIndex, " mLastViaShowIndex:", mLastViaShowIndex);
        }
        // 移动车标的Y坐标
        NaviUiUtil.setTranslation(mViewBinding.sivCar, carPosition, mIsHorizontal);
        ArrayList<Integer> chargeShowList = new ArrayList<>();
        int chargeShowIndex = 0;// 充电站显示索引
        // 绘制充电站
        if (mChargeStationRemain != null && !mChargeStationRemain.isEmpty() && mIsShowAutoAddChargeStation) {
            StringBuilder logBuilder = null;
            if (Logger.openLog) {
                logBuilder = new StringBuilder();
            }
            int preVia = 0;
            for (int i = 0; i < mChargeStationRemain.size() && i < MAX_VIA_NUM; i++) {
                final NaviEtaInfo.NaviTimeAndDist viaItem = mChargeStationRemain.get(i);
                int via;
                if (mIsHorizontal) {
                    via = Math.round((hasPassedDistance + mDistanceHasPassed + viaItem.dist) * rateDistanceToView);
                } else {
                    via = Math.round((mTotalDistance - hasPassedDistance - mDistanceHasPassed - viaItem.dist) * rateDistanceToView);
                }
                if (Logger.openLog && logBuilder != null) {
                    logBuilder.append(" ").append(i).append(".via:").append(via);
                }
                via = getChargeCheckedVia(i, via);
                if (Logger.openLog && logBuilder != null) {
                    logBuilder.append(".checkVia:").append(via);
                }
                if (via == -1) {
                    continue;
                }
                boolean isShow = false;
                if ((via - preVia > VIA_WIDTH || preVia == 0)) {
                    if (via + VIA_WIDTH > width) {//光柱图最后一个扎点显示不全
                        if (preVia + 2 * VIA_WIDTH > width) {//如果前一个扎点+2个宽度大于光柱图宽度空间不足，则不显示
                            if (Logger.openLog && logBuilder != null) {
                                logBuilder.append(".lastViaNotShow");
                            }
                            continue;
                        } else {
                            via = width - VIA_WIDTH;//如果空间足够则往前移动
                        }
                    }
                    isShow = true;
                    chargeShowList.add(via);
                    preVia = via;
                    showViaIcon(chargeShowIndex, SceneCommonStruct.TmcViaPointType.ViaChargeType, via);
                    chargeShowIndex++;
                }
                if (Logger.openLog && logBuilder != null) {
                    logBuilder.append(".show:").append(isShow);
                }
            }
            if (chargeShowIndex > 0) {//为0说明未更新
                mLastChargeShowIndex = chargeShowIndex;
                if (Logger.openLog) {
                    Logger.d(TAG, "绘制充电站 mChargeShowIndex:", chargeShowIndex, logBuilder);
                }
            }
        } else {
            // 不显示充电站
            if (mLastChargeShowIndex > 0) {
                if (Logger.openLog) {
                    Logger.d(TAG, "不显示充电站 mLastChargeShowIndex:", mLastChargeShowIndex);
                }
                for (int i = 0; i < mLastChargeShowIndex && i < MAX_VIA_NUM; i++) {
                    hideViaIcon(i);
                }
                mLastChargeShowIndex = 0;
            }
        }

        // 绘制途径点
        if (mViaRemain != null && !mViaRemain.isEmpty()) {
            int viaShowIndex = mLastChargeShowIndex;
            StringBuilder logBuilder = null;
            if (Logger.openLog) {
                logBuilder = new StringBuilder();
            }
            int preVia = 0;
            for (int i = 0; i < mViaRemain.size() && i < MAX_VIA_NUM; i++) {
                final NaviEtaInfo.NaviTimeAndDist viaItem = mViaRemain.get(i);
                int via;
                boolean isShow = false;
                if (mIsHorizontal) {
                    via = Math.min(Math.round((hasPassedDistance + mDistanceHasPassed + viaItem.dist) * rateDistanceToView), width - mViaWidth);
                } else {
                    via = Math.round((mTotalDistance - hasPassedDistance - mDistanceHasPassed - viaItem.dist) * rateDistanceToView);
                }
                if (Logger.openLog && logBuilder != null) {
                    logBuilder.append(" ").append(mLastChargeShowIndex + i).append(".via:").append(via);
                }
                via = getRemainCheckedVia(i, via);
                if (via == -1) {
                    continue;
                }
                if (mISceneCallback != null && mISceneCallback.getIsViaArrived() && i == 0) {
                    //如果是途经点手动到达了，则不显示第一个途经点
                    if (Logger.openLog && logBuilder != null) {
                        logBuilder.append(".ViaArrived");
                    }
                    continue;
                }
                if (Logger.openLog && logBuilder != null) {
                    logBuilder.append(".checkVia:").append(via);
                }
                if ((via - preVia > VIA_WIDTH || preVia == 0)) {
                    boolean isOverlapping = false;//是否与充电站显示扎点冲突
                    for (Integer item : chargeShowList) {
                        if (Math.abs(item - via) < VIA_WIDTH) {
                            isOverlapping = true;
                            break;
                        }
                    }
                    if (!isOverlapping) {
                        if (via + VIA_WIDTH > width) {//光柱图最后一个扎点显示不全
                            if (preVia + 2 * VIA_WIDTH > width) {//如果前一个扎点+2个宽度大于光柱图宽度空间不足，则不显示
                                if (Logger.openLog && logBuilder != null) {
                                    logBuilder.append(".lastViaNotShow");
                                }
                                continue;
                            } else {
                                via = width - VIA_WIDTH;//如果空间足够则往前移动
                            }
                        }
                        isShow = true;
                        preVia = via;
                        showViaIcon(viaShowIndex, SceneCommonStruct.TmcViaPointType.ViaPointType, via);
                        viaShowIndex++;
                    }
                }
                if (Logger.openLog && logBuilder != null) {
                    logBuilder.append(".show:").append(isShow);
                }
            }
            if (viaShowIndex > mLastChargeShowIndex) {
                mLastViaShowIndex = viaShowIndex;
                if (Logger.openLog) {
                    Logger.d(TAG, "绘制途经点 viaShowIndex:", viaShowIndex, " mLastChargeShowIndex:", mLastChargeShowIndex, logBuilder);
                }
            }
        } else {
            // 不显示途经点
            if (mLastViaShowIndex - mLastChargeShowIndex > 0) {
                if (Logger.openLog) {
                    Logger.d(TAG, "不显示途经点 mLastViaShowIndex:", mLastViaShowIndex, " mLastChargeShowIndex:", mLastChargeShowIndex);
                }
                for (int i = mLastChargeShowIndex; i < mLastViaShowIndex && i < MAX_VIA_NUM; i++) {
                    hideViaIcon(i);
                }
                mLastViaShowIndex = mLastChargeShowIndex;
            }
        }
    }

    /**
     * 获取光柱图充电站扎点抖动修正后的Via值
     * @param index
     * @param via
     * @return
     */
    private int getChargeCheckedVia(int index, int via) {
        if (mViaChargeArr == null || mViaChargeArr[0] != mTotalDistance || mViaChargeArr[1] != mChargeStationRemain.size()) {
            mViaChargeArr = new long[2 + mChargeStationRemain.size()];
            mViaChargeArr[0] = mTotalDistance;
            mViaChargeArr[1] = mChargeStationRemain.size();
            mViaChargeArr[2 + index] = via;
            return via;
        } else if(mViaChargeArr[2 + index] == 0){
            mViaChargeArr[2 + index] = via;
            return via;
        }
        //return (int) mViaChargeArr[2 + index];
        return -1;//信息不变无需刷新
    }

    private boolean isViaChange(final long totalDistance) {
        if (mChargeStationRemain == null || mViaRemain == null) {
            return true;
        }
        if (mTotalDistance != totalDistance || mLastChargeSize != mChargeStationRemain.size()
                || mLastViaSize != mViaRemain.size()) {
            mLastChargeSize = mChargeStationRemain.size();
            mLastViaSize = mViaRemain.size();
            return true;
        }
        return false;
    }

    /**
     * 获取光柱图途经点扎点抖动修正后的Via值
     * @param index
     * @param via
     * @return
     */
    private int getRemainCheckedVia(int index, int via) {
        if (mViaRemainArr == null || mViaRemainArr[0] != mTotalDistance || mViaRemainArr[1] != mViaRemain.size()) {
            mViaRemainArr = new long[2 + mViaRemain.size()];
            mViaRemainArr[0] = mTotalDistance;
            mViaRemainArr[1] = mViaRemain.size();
            mViaRemainArr[2 + index] = via;
            return via;
        } else if(mViaRemainArr[2 + index] == 0){
            mViaRemainArr[2 + index] = via;
            return via;
        }
        //return (int) mViaRemainArr[2 + index];
        return -1;//信息不变无需刷新
    }

    /**
     * 设置途径点和充电桩数据
     *
     * @param viaRemain           剩余的途经点
     * @param chargeStationRemain 剩余的充电站
     */
    public void updateTmcVia(final ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain,
                             final ArrayList<NaviEtaInfo.NaviTimeAndDist> chargeStationRemain) {
        if (viaRemain != null && mViaRemain != null && viaRemain.size() != mViaRemain.size()) {
            Logger.i(TAG, "updateTmcVia resetView");
            resetView();
        }
        mViaRemain = viaRemain;
        if (chargeStationRemain != null && mChargeStationRemain != null && chargeStationRemain.size() != mChargeStationRemain.size()) {
            Logger.i(TAG, "updateTmcVia ChargeStationRemain resetView");
            resetView();
        }
        mChargeStationRemain = chargeStationRemain;
    }

    /**
     * 重置视图
     */
    public void resetView() {
        Logger.i(TAG, "resetView");
        if (mViewBinding == null) {
            return;
        }
        mLastChargeSize = 0;
        mLastViaSize = 0;
        NaviUiUtil.hideView(mViewBinding.siv1, mViewBinding.siv2, mViewBinding.siv31, mViewBinding.siv32, mViewBinding.siv33,
                mViewBinding.siv34, mViewBinding.siv35, mViewBinding.siv36, mViewBinding.siv37, mViewBinding.siv38,
                mViewBinding.siv39, mViewBinding.siv310, mViewBinding.siv311, mViewBinding.siv312, mViewBinding.siv313,
                mViewBinding.siv314, mViewBinding.siv315, mViewBinding.siv316, mViewBinding.siv317, mViewBinding.siv3);
    }

    /**
     * @param view            view
     * @param tmcViaPointType 类型
     * @param viaY            viaY
     */
    private void showViaIcon(final View view,
                             final SceneCommonStruct.TmcViaPointType tmcViaPointType,
                             final int viaY) {
        if (!(BuildConfig.FLAVOR.equals("clea_8255")
                || BuildConfig.FLAVOR.equals("clea_local_8155")
                || BuildConfig.FLAVOR.equals("clea_8775"))) {
            if (view instanceof SkinTextView stv) {
                if (tmcViaPointType == SceneCommonStruct.TmcViaPointType.ViaChargeType) {
                    stv.setText(getContext().getText(R.string.navi_via_item_charge));
                }
            }
        }
        NaviUiUtil.showView(view);
        setViaBackground(view, tmcViaPointType);
        NaviUiUtil.setTranslation(view, viaY, mIsHorizontal);
    }

    /**
     * 显示途径点图标
     *
     * @param index           index
     * @param tmcViaPointType 类型
     * @param viaY            viaY
     */
    private void showViaIcon(final int index,
                             final SceneCommonStruct.TmcViaPointType tmcViaPointType,
                             final int viaY) {
        Logger.d(TAG, "showViaIcon size:", mViaRemain.size(), ",index :", index,
                ",tmcViaPointType：", tmcViaPointType, ",viaY：", viaY);
        switch (index) {
            case NaviConstant.TMCViaIndex.VIA_0:
                showViaIcon(mViewBinding.siv1, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_1:
                showViaIcon(mViewBinding.siv2, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_2:
                showViaIcon(mViewBinding.siv31, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_3:
                showViaIcon(mViewBinding.siv32, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_4:
                showViaIcon(mViewBinding.siv33, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_5:
                showViaIcon(mViewBinding.siv34, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_6:
                showViaIcon(mViewBinding.siv35, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_7:
                showViaIcon(mViewBinding.siv36, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_8:
                showViaIcon(mViewBinding.siv37, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_9:
                showViaIcon(mViewBinding.siv38, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_10:
                showViaIcon(mViewBinding.siv39, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_11:
                showViaIcon(mViewBinding.siv310, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_12:
                showViaIcon(mViewBinding.siv311, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_13:
                showViaIcon(mViewBinding.siv312, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_14:
                showViaIcon(mViewBinding.siv313, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_15:
                showViaIcon(mViewBinding.siv314, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_16:
                showViaIcon(mViewBinding.siv315, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_17:
                showViaIcon(mViewBinding.siv316, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_18:
                showViaIcon(mViewBinding.siv317, tmcViaPointType, viaY);
                break;
            case NaviConstant.TMCViaIndex.VIA_19:
                showViaIcon(mViewBinding.siv3, tmcViaPointType, viaY);
                break;
            default:
                break;
        }
    }

    /**
     * 隐藏途径点图标
     *
     * @param index index
     */
    private void hideViaIcon(final int index) {
        Logger.d(TAG, "hideViaIcon index :", index);
        switch (index) {
            case NaviConstant.TMCViaIndex.VIA_0:
                NaviUiUtil.hideView(mViewBinding.siv1);
                break;
            case NaviConstant.TMCViaIndex.VIA_1:
                NaviUiUtil.hideView(mViewBinding.siv2);
                break;
            case NaviConstant.TMCViaIndex.VIA_2:
                NaviUiUtil.hideView(mViewBinding.siv31);
                break;
            case NaviConstant.TMCViaIndex.VIA_3:
                NaviUiUtil.hideView(mViewBinding.siv32);
                break;
            case NaviConstant.TMCViaIndex.VIA_4:
                NaviUiUtil.hideView(mViewBinding.siv33);
                break;
            case NaviConstant.TMCViaIndex.VIA_5:
                NaviUiUtil.hideView(mViewBinding.siv34);
                break;
            case NaviConstant.TMCViaIndex.VIA_6:
                NaviUiUtil.hideView(mViewBinding.siv35);
                break;
            case NaviConstant.TMCViaIndex.VIA_7:
                NaviUiUtil.hideView(mViewBinding.siv36);
                break;
            case NaviConstant.TMCViaIndex.VIA_8:
                NaviUiUtil.hideView(mViewBinding.siv37);
                break;
            case NaviConstant.TMCViaIndex.VIA_9:
                NaviUiUtil.hideView(mViewBinding.siv38);
                break;
            case NaviConstant.TMCViaIndex.VIA_10:
                NaviUiUtil.hideView(mViewBinding.siv39);
                break;
            case NaviConstant.TMCViaIndex.VIA_11:
                NaviUiUtil.hideView(mViewBinding.siv310);
                break;
            case NaviConstant.TMCViaIndex.VIA_12:
                NaviUiUtil.hideView(mViewBinding.siv311);
                break;
            case NaviConstant.TMCViaIndex.VIA_13:
                NaviUiUtil.hideView(mViewBinding.siv312);
                break;
            case NaviConstant.TMCViaIndex.VIA_14:
                NaviUiUtil.hideView(mViewBinding.siv313);
                break;
            case NaviConstant.TMCViaIndex.VIA_15:
                NaviUiUtil.hideView(mViewBinding.siv314);
                break;
            case NaviConstant.TMCViaIndex.VIA_16:
                NaviUiUtil.hideView(mViewBinding.siv315);
                break;
            case NaviConstant.TMCViaIndex.VIA_17:
                NaviUiUtil.hideView(mViewBinding.siv316);
                break;
            case NaviConstant.TMCViaIndex.VIA_18:
                NaviUiUtil.hideView(mViewBinding.siv317);
                break;
            case NaviConstant.TMCViaIndex.VIA_19:
                NaviUiUtil.hideView(mViewBinding.siv3);
                break;
            default:
                break;
        }
    }

    /**
     * @param view view
     * @param type 类型
     */
    public static void setViaBackground(final View view,
                                        final SceneCommonStruct.TmcViaPointType type) {
        view.setBackgroundResource(SceneEnumRes.getDrawableEnumName0(type).getDayDrawableId());
    }

    /**
     * 更新光柱图数据view
     *
     * @param tbitem            item
     * @param distanceHasPassed 已经经过的距离
     * @param totalDistance     总距离
     */
    public void updateTmcAreaNew(final List<NaviTmcInfo.NaviTmcInfoData> tbitem,
                                 final long distanceHasPassed, final long totalDistance) {
        mViewBinding.tmrtrResources.updateTmcAreaNew(tbitem, distanceHasPassed, totalDistance);
    }

    /**
     * @param offline 是否离线
     */
    public void setOffline(final boolean offline) {
        mOffline = offline;
        if (mOffline) {
//            mViewBinding.carBottomLine.setImageResource(R.color.auto_color_0a80fb);
        } else {
//            mViewBinding.carBottomLine.setImageResource(R.color.auto_color_ca6d);
        }
//        mViewBinding.tmrtrResources.setOffline(offline);
    }

    public void setIsShowAutoAdd(boolean isShow) {
        if (isShow != mIsShowAutoAddChargeStation) {
            Logger.d(TAG, " isShow:", isShow);
            mViaRemainArr = null;
            mViaChargeArr = null;
        }
        this.mIsShowAutoAddChargeStation = isShow;
        mScreenViewModel.innerUpdateNaviInfo();
    }

    /**
     * 是否强制更新 光柱图
     *
     * @return true 强制更新
     */
    public boolean isForceUpdate() {
        if (mViewBinding == null) {
            return false;
        }
        boolean isForceUpdate = mViewBinding.tmrtrResources.isForceUpdate();
        if (isForceUpdate) {
            mInvalidateCount = 0;
        }
        return isForceUpdate;
    }

}
