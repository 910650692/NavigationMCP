package com.fy.navi.scene.ui.navi;


import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.BitmapDrawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviTbtViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviTbtImpl;
import com.fy.navi.scene.impl.navi.common.AutoUIDrawable;
import com.fy.navi.scene.impl.navi.common.AutoUIString;
import com.fy.navi.scene.impl.navi.common.SceneCommonStruct;
import com.fy.navi.scene.impl.navi.common.SceneEnumRes;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.utils.NumberUtils;

/**
 * tbt看板scene
 *
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviTbtView extends NaviSceneBase<SceneNaviTbtViewBinding, SceneNaviTbtImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private int mGpsStrength;

    private boolean mIsCrossImageShow;

    private long mCrossDistance = NumberUtils.NUM_ERROR;

    public SceneNaviTbtView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviTbtView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviTbtView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                            final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_TBT;
    }

    @Override
    protected SceneNaviTbtViewBinding createViewBinding(final LayoutInflater inflater,
                                                        final ViewGroup viewGroup) {
        return SceneNaviTbtViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviTbtImpl initSceneImpl() {
        return new SceneNaviTbtImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviTbt(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }

    /**
     * @param naviETAInfo naviInfo
     */
    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviETAInfo);
        }
    }

    /**
     * @param info 转向信息
     */
    public void onManeuverInfo(final NaviManeuverInfo info) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onManeuverInfo(info);
        }
    }

    /**
     * @param traceId  traceId
     * @param naviType 导航类型
     */
    public void onNaviArrive(final long traceId, final int naviType) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviArrive(traceId, naviType);
        }
    }

    @Override
    public void onDestroy() {
        mISceneCallback = null;
    }

    /**
     * 设置引导转向图片
     *
     * @param drawableValue drawable
     */
    public void setBackgroundNaviExitTurnIcon(final AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviExitTurnIcon：");
        // 《出口信息》图标 ui未体现
        mViewBinding.sivHudSou33.setBackground(new BitmapDrawable(getResources(), drawableValue.getBitmap()));
    }

    /**
     * 设置出口提示
     *
     * @param textContent 文本内容
     */
    public void setTextNaviExit(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviExit：" + textContent.getString(getContext()));
        // 《出口信息》出口编号 ui未体现
        mViewBinding.stvExitNum.setText(textContent.getString(getContext()));
    }

    /**
     * 设置到下一路口名称
     *
     * @param textContent 文本内容
     */
    public void setTextNaviInfoDistanceNextRoad(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoad：" + textContent.getString(getContext()));
        mViewBinding.stvDivDistance.setText(textContent.getString(getContext()));
    }

    /**
     * @param textContent 文本内容
     */
    public void setTextNaviInfoDistanceNextRoadName(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoadName：" +
                textContent.getString(getContext()));
        mViewBinding.stvDivAddress.setText(textContent.getString(getContext()));
    }

    /**
     * 设置引导普通转向图片
     *
     * @param drawableValue drawable
     */
    public void setBackgroundNaviCommonTurnIcon(final AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviCommonTurnIcon：");
        mViewBinding.sivHudSou3.setBackground(new BitmapDrawable(getResources(), drawableValue.getBitmap()));
    }

    /**
     * 设置到下一路口剩余距离单位
     *
     * @param textContent 文本内容
     */
    @SuppressLint("SetTextI18n")
    public void setTextNaviInfoDistanceNextRoadUnit(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoadUnit：" +
                textContent.getString(getContext()));
        mViewBinding.stvDivUnit.setText(textContent.getString(getContext()));
    }

    /**
     * 设置离线引导普通转向图片
     *
     * @param iconAction iconAction
     */
    public void setBackgroundNaviOfflineCommonTurnIcon(
            final SceneCommonStruct.TbtIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineCommonTurnIcon：" +
                iconAction.name());
        mViewBinding.sivHudSou3.setBackgroundResource(SceneEnumRes.getDrawableEnumName(iconAction).
                getDayDrawableId());
    }

    /**
     * 设置离线引导出口转向图片
     *
     * @param iconAction iconAction
     */
    public void setBackgroundNaviOfflineExitTurnIcon(
            final SceneCommonStruct.TbtExitIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineExitTurnIcon：" +
                iconAction.name());
        // 《出口信息》图标 ui未体现
        mViewBinding.sivHudSou33.setBackgroundResource(SceneEnumRes.getDrawableEnumName(iconAction).
                getDayDrawableId());
    }

    public void onUpdateGpsStrength(int gpsStrength) {
        if (mGpsStrength == gpsStrength) {
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            mGpsStrength = gpsStrength;
            switch (gpsStrength) {
                case NaviConstant.GpsStrengthState.GPS_NONE:
                    mViewBinding.sivGps.setBackgroundResource(R.drawable.img_satellite_none_42);
                    break;
                case NaviConstant.GpsStrengthState.GPS_STRONG:
                    mViewBinding.sivGps.setBackgroundResource(R.drawable.img_satellite_42);
                    break;
                case NaviConstant.GpsStrengthState.GPS_MEDIUM:
                    mViewBinding.sivGps.setBackgroundResource(R.drawable.img_satellite_medium_42);
                    break;
                case NaviConstant.GpsStrengthState.GPS_WEAK:
                    mViewBinding.sivGps.setBackgroundResource(R.drawable.img_satellite_weak_42);
                    break;
            }
        });
    }

    public void updateCrossProgress(long routeRemainDist) {
        Logger.i(TAG, "updateCrossProgress routeRemainDist:" + routeRemainDist);
        if (mIsCrossImageShow) {
            int width = calculateProgressBarLength(mCrossDistance, routeRemainDist);
            ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
                    mViewBinding.sivProgress.getLayoutParams();
            params.width = width;
            mViewBinding.sivProgress.setLayoutParams(params);
            if (width == 640) {
                mViewBinding.sivProgress.setBackgroundResource(R.drawable.bg_navi_tbt_progress_full);
            } else {
                mViewBinding.sivProgress.setBackgroundResource(R.drawable.bg_navi_tbt_progress);
            }
            mViewBinding.sivProgress.requestLayout();
        }
    }

    public void onCrossImageInfo(boolean isRealNeedShow, CrossImageEntity naviImageInfo) {
        Logger.i(TAG, "onCrossImageInfo isRealNeedShow:" + isRealNeedShow +
                " mCrossDistance:" + mCrossDistance + " naviImageInfo:" +
                (naviImageInfo == null ? 0 : naviImageInfo.getDistance()));
        mIsCrossImageShow = isRealNeedShow;
        if (!mIsCrossImageShow) {
            resetProgress();
        }
        mCrossDistance = naviImageInfo == null ? 0 : naviImageInfo.getDistance();
    }

    public void onCrossImageInfo(boolean isRealNeedShow) {
        mIsCrossImageShow = isRealNeedShow;
        if (!mIsCrossImageShow) {
            resetProgress();
        } else {
            updateTbtHeight(101, 4, 35);
        }
    }

    private void resetProgress() {
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams) mViewBinding.
                        sivProgress.getLayoutParams();
                params.width = 0;
                mViewBinding.sivProgress.setLayoutParams(params);
                mViewBinding.sivProgress.requestLayout();
                updateTbtHeight(119, 16, 30);
            }
        });
    }

    /**
     * 因为路口大图出现的时候TBT布局和正常状态下的不一致，所以需要动态修改
     * @param height 布局高度
     * @param gpsTopMargin gps图标上边距
     */
    private void updateTbtHeight(int height, int gpsTopMargin, int guideEnd) {
        ThreadManager.getInstance().postUi(() -> {
            LayoutParams params = (LayoutParams) mViewBinding.
                    sclProgress.getLayoutParams();
            params.height = height;
            mViewBinding.sclProgress.setLayoutParams(params);
            mViewBinding.sclProgress.requestLayout();
            params = (LayoutParams) mViewBinding.sivGps.getLayoutParams();
            params.topMargin = gpsTopMargin;
            mViewBinding.sivGps.setLayoutParams(params);
            mViewBinding.sivGps.requestLayout();
            params = (LayoutParams) mViewBinding.glRouteName.getLayoutParams();
            params.guideEnd = guideEnd;
            mViewBinding.glRouteName.setLayoutParams(params);
            mViewBinding.glRouteName.requestLayout();
        });
    }

    /**
     * 计算进度条长度
     * @param totalDistance 总距离（long类型）
     * @param remainingDistance 剩余距离（long类型）
     * @return 进度条长度（int类型，范围0-640）
     */
    private int calculateProgressBarLength(long totalDistance, long remainingDistance) {
        // 参数校验
        if (totalDistance <= 0) {
            Logger.i(TAG,"总距离必须大于0");
            return 0;
        }

        if (remainingDistance < 0) {
            Logger.i(TAG,"剩余距离不能为负数");
            return 0;
        }

        if (remainingDistance > totalDistance) {
            Logger.i(TAG,"剩余距离不能大于总距离");
            return 640;
        }

        // 计算已完成的比例（0.0-1.0）
        double progressRatio = 1.0 - ((double) remainingDistance / totalDistance);

        // 确保比例在0-1范围内（处理浮点运算可能的小数误差）
        progressRatio = Math.max(0.0, Math.min(1.0, progressRatio));

        // 计算进度条长度（最大640dp）
        int progressLength = (int) Math.round(640 * progressRatio);

        // 确保返回值为0-640之间的整数
        return Math.max(0, Math.min(640, progressLength));
    }
}
