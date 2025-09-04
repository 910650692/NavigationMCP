package com.sgm.navi.scene.impl.navi;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.ui.navi.SceneNaviParallelView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.position.LocParallelInfoEntity;
import com.sgm.navi.service.define.position.LocalParallelRoadEntity;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.action.Action;

import java.math.BigInteger;
import java.util.ArrayList;

public class SceneNaviParallelImpl extends BaseSceneModel<SceneNaviParallelView> implements IPositionPackageCallback{
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_PARALLEL_IMPL;
    private final PositionPackage mPositionPackage;
    private LocParallelInfoEntity mCurrentParallelRoadInfo;
    private LocParallelInfoEntity mPreviousParallelRoadInfo;
    /***按钮点击动作类型***/
    private int mSwitchRoadType;
    private int mSwitchBridgeType;
    private int mSwitchActionType = NaviConstant.LocSwitchRoadType.LOC_SWITCH_NULL;
    private boolean mIsSwitchParallelEnabled = true;
    public static final int PARALLELROAD_FLAG_MAIN = 1;
    public static final int PARALLELROAD_FLAG_SIDE = 2;
    public static final int HW_FLAG_UP = 1;
    public static final int HW_FLAG_DOWN = 2;
    // 上下桥切换按钮是否可见
    public ObservableField<Boolean> mBridgeUpDownVisible;
    // 主辅路切换按钮是否可见
    public ObservableField<Boolean> mRoadMainAuxiliaryVisible;

    /**
     * @param screenView screenView
     */
    public SceneNaviParallelImpl(final SceneNaviParallelView screenView) {
        super(screenView);
        mPositionPackage = PositionPackage.getInstance();
        mPositionPackage.registerCallBack(this);
        mBridgeUpDownVisible = new ObservableField<>(false);
        mRoadMainAuxiliaryVisible = new ObservableField<>(false);
    }

    public Action mClickBridgeUpDown = () -> {
        Logger.i(TAG, "clickBridgeUpDown ");
        mSwitchActionType = mSwitchBridgeType;
        requestSwitchParallelRoad();
    };

    public Action mClickMainAuxiliary = () -> {
        Logger.i(TAG, "clickMainAuxiliary ");
        mSwitchActionType = mSwitchRoadType;
        requestSwitchParallelRoad();
    };

    @Override
    protected void onDestroy() {
        mPositionPackage.unregisterCallBack(this);
        super.onDestroy();
    }

    /**
     * 请求切换平行路
     */
    private void requestSwitchParallelRoad() {
        Logger.i("mIsSwitchParallelEnabled= " + mIsSwitchParallelEnabled +
                ",mSwitchActionType：" + mSwitchActionType);
        if (!mIsSwitchParallelEnabled) {
            return;
        }
        // 网络正常就进行正常路算
        if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            final BigInteger parallelRoadId = getOffLineParallelRoadId(mSwitchActionType);
            mPositionPackage.switchParallelRoad(mSwitchActionType, parallelRoadId);
        } else {
            Logger.i(TAG, "网络异常，进行离线平行路切换");
            // 离线不支持平行路的桥梁切换，这里加判断容错
            if (mSwitchActionType ==
                    NaviConstant.LocSwitchRoadType.LOC_SWITCH_DOWN_BRIDGE_TO_UP_BRIDGE ||
                    mSwitchActionType ==
                            NaviConstant.LocSwitchRoadType.LOC_SWITCH_UP_BRIDGE_TO_DOWN_BRIDGE) {
                Logger.i(TAG, "offline can not switch bridge");
                return;
            }
            final BigInteger parallelRoadId = getOffLineParallelRoadId(mSwitchActionType);
            mPositionPackage.switchParallelRoad(mSwitchActionType, parallelRoadId);
        }
        mIsSwitchParallelEnabled = false;
    }

    /**
     * 获取离线切换的平行路道路ID
     *
     * @param switchActionType 平行路切换类型 -1：不切换 0:切换到辅路 1:切换到主路
     * @return 离线切换的平行路道路ID
     */
    private BigInteger getOffLineParallelRoadId(final int switchActionType) {
        return switch (switchActionType) {
            case NaviConstant.LocSwitchRoadType.LOC_SWITCH_MAIN_TO_SIDE -> getOffLineSideRoadId();
            case NaviConstant.LocSwitchRoadType.LOC_SWITCH_SIDE_TO_MAIN -> getOffLineMainRoadId();
            default -> BigInteger.ZERO;
        };
    }

    /**
     * 获取离线模式下的辅路道路ID
     * @return 离线切换的辅路道路ID
     */
    private BigInteger getOffLineSideRoadId() {
        if (mCurrentParallelRoadInfo != null) {
            final ArrayList<LocalParallelRoadEntity> list = mCurrentParallelRoadInfo.
                    getLocalParallelRoadArrayList();
            if (list != null && !list.isEmpty()) {
                for (LocalParallelRoadEntity entity : list) {
                    if (entity.getType() == LocalParallelRoadEntity.SIDE_ROAD) {
                        return entity.getRoadID();
                    }
                }
            }
        }
        return BigInteger.ZERO;
    }

    /**
     * 获取离线模式下的主路道路ID
     * @return 离线切换的主路道路ID
     */
    private BigInteger getOffLineMainRoadId() {
        if (mCurrentParallelRoadInfo != null) {
            final ArrayList<LocalParallelRoadEntity> list = mCurrentParallelRoadInfo.
                    getLocalParallelRoadArrayList();
            if (list != null && !list.isEmpty()) {
                for (LocalParallelRoadEntity entity : list) {
                    if (entity.getType() == LocalParallelRoadEntity.MAIN_ROAD) {
                        return entity.getRoadID();
                    }
                }
            }
        }
        return BigInteger.ZERO;
    }

    @Override
    public void onSwitchParallelRoadFinished() {
        Logger.i(TAG, " 平行路切换结束 需要重算路  mCurrentParallelRoadInfo:", (mCurrentParallelRoadInfo != null));
        if (mCurrentParallelRoadInfo != null) {
            LocInfoBean locInfoBean = mPositionPackage.getLastCarLocation();
            final ArrayList<LocalParallelRoadEntity> list = mCurrentParallelRoadInfo.getLocalParallelRoadArrayList();
            BigInteger roadID = BigInteger.ZERO;
            if(list != null && !list.isEmpty()){
                LocalParallelRoadEntity entity = list.get(0);
                if(entity != null){
                    roadID = entity.getRoadID();
                }
            }
            int flag = mCurrentParallelRoadInfo.getFlag();
            int hwFlag = mCurrentParallelRoadInfo.getHwFlag();
            RoutePackage.getInstance().clearRestrictPolyline(MapType.MAIN_SCREEN_MAIN_MAP);
            RoutePackage.getInstance().requestSwitchParallelRoute(mSwitchRoadType,locInfoBean, roadID, (short) flag, (short) hwFlag);
        }
        mIsSwitchParallelEnabled = true;
    }

    /**
     * status 主辅路切换状态:0 非平行路切换期间 1 平行路切换期间
     *
     * flag 主辅路标识（默认0，离线数据计算/在线算路下发）
     * 0：无主辅路（车标所在道路旁无主辅路）
     * 1：车标在主路（车标所在道路旁有辅路）
     * 2：车标在辅路（车标所在道路旁有主路）
     *
     * hwFlag < 高架上下标识（默认0，在线算路下发）
     *  0：无高架
     *  1：车标在高架上（车标所在道路有对应高架下）
     *  2：车标在高架下（车标所在道路有对应高架上）
     */
    @Override
    public void onParallelRoadUpdate(LocParallelInfoEntity entity) {
        if (mCallBack != null) {
            mCallBack.setLocParallelInfoEntity(entity);
        }
        // 如果是模拟导航的话不显示平行路切换按钮
        if (ConvertUtils.isNull(entity) || NaviPackage.getInstance().getCurrentNaviType() == NumberUtils.NUM_1) {
            return;
        }
        Logger.e(TAG, "平行路切换 算路结果 ", entity.toString());
        // 如果在非平行路切换期间，可以切换主辅路
        if (entity.getStatus() == NumberUtils.NUM_0) {
            mIsSwitchParallelEnabled = true;
        }
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                if (mCallBack == null) {
                    Logger.i(TAG, "平行路切换  mCallBack is null");
                    return;
                }
                if (ConvertUtils.isEmpty(entity)) {
                    Logger.i(TAG, "平行路切换 entity is null");
                    hideSwitchParallelRoadUi();
                    return;
                }
                mCurrentParallelRoadInfo = entity;
                refreshRoadBridgeUi();
            }
        });
    }

    /**
     * 隐藏平行路切换UI
     */
    private void hideSwitchParallelRoadUi() {
        mSwitchRoadType = NaviConstant.LocSwitchRoadType.LOC_SWITCH_NULL;
        mSwitchBridgeType = NaviConstant.LocSwitchRoadType.LOC_SWITCH_NULL;
        mRoadMainAuxiliaryVisible.set(false);
        mBridgeUpDownVisible.set(false);
        updateSceneVisible(false);
    }

    /**
     * 刷新主辅路和高架上下UI
     */
    private void refreshRoadBridgeUi() {
        if (mCallBack == null) {
            Logger.i(TAG, "平行路切换  mCallBack is null");
            return;
        }
        // 平行路切换功能隐藏
        if (mCurrentParallelRoadInfo == null) {
            Logger.i(TAG, "平行路切换 mCurrent is null");
            mRoadMainAuxiliaryVisible.set(false);
            mBridgeUpDownVisible.set(false);
            updateSceneVisible(false);
            return;
        }
        if (mPreviousParallelRoadInfo != null) {
            // 如果主辅路和桥上下没有变化，直接返回
            if (mPreviousParallelRoadInfo.getFlag() == mCurrentParallelRoadInfo.getFlag() &&
                    mPreviousParallelRoadInfo.getHwFlag() == mCurrentParallelRoadInfo.getHwFlag() &&
                    mPreviousParallelRoadInfo.getStatus() == mCurrentParallelRoadInfo.getStatus()) {
                Logger.i(TAG, "平行路切换 主辅路和桥上下没有变化，直接返回 mPrevious：", mPreviousParallelRoadInfo.toString());
                return;
            }
            Logger.i(TAG, "平行路切换 CurStatus:", mCurrentParallelRoadInfo.getStatus(), " CurFlag:", mCurrentParallelRoadInfo.getFlag(),
                    " CurHwFlag:", mCurrentParallelRoadInfo.getHwFlag(), " PreStatus:", mPreviousParallelRoadInfo.getStatus(), " PreFlag:",
                    mPreviousParallelRoadInfo.getFlag(), " PreHwFlag:", mPreviousParallelRoadInfo.getHwFlag());
        } else {
            Logger.i(TAG, "平行路切换 Pre null CurStatus:", mCurrentParallelRoadInfo.getStatus(), " CurFlag:",
                    mCurrentParallelRoadInfo.getFlag(), " CurHwFlag:", mCurrentParallelRoadInfo.getHwFlag());
        }
        // 保存并刷新当前主辅路、桥上下
        mPreviousParallelRoadInfo = mCurrentParallelRoadInfo;
        hideSwitchParallelRoadUi();

        // 不显示按钮的场景：切换过程中、断网场景、车道级导航中
        if (1 == mCurrentParallelRoadInfo.getStatus()) {
            Logger.d(TAG, "不显示按钮：切换过程中、断网场景、车道级导航中");
            return;
        }

        // 显示切换到辅路的按钮
        if (mCurrentParallelRoadInfo.getFlag() == PARALLELROAD_FLAG_MAIN) {
            Logger.i(TAG, "当前在主路，显示切换到辅路的按钮");
            mScreenView.sceneRoadSide();
            mSwitchRoadType = NaviConstant.LocSwitchRoadType.LOC_SWITCH_MAIN_TO_SIDE;
            mRoadMainAuxiliaryVisible.set(true);
            updateSceneVisible(true);
            //显示切换到主路的按钮
        } else if (mCurrentParallelRoadInfo.getFlag() == PARALLELROAD_FLAG_SIDE) {
            Logger.i(TAG, "当前在辅路，显示切换到主路的按钮");
            mScreenView.sceneRoadMain();
            mSwitchRoadType = NaviConstant.LocSwitchRoadType.LOC_SWITCH_SIDE_TO_MAIN;
            mRoadMainAuxiliaryVisible.set(true);
            updateSceneVisible(true);
        }
        // 显示切换到桥下的按钮
        if (mCurrentParallelRoadInfo.getHwFlag() == HW_FLAG_UP) {
            Logger.i(TAG, "当前在桥上，显示切换到桥下的按钮");
            mScreenView.sceneBridgeDown();
            mSwitchBridgeType = NaviConstant.LocSwitchRoadType.LOC_SWITCH_UP_BRIDGE_TO_DOWN_BRIDGE;
            updateSceneVisible(true);
            // 如果网络正常，才显示桥上下按钮
            if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                mBridgeUpDownVisible.set(true);
            } else {
                Logger.i(TAG, "网络异常，不显示桥上下按钮");
            }
            // 显示切换到桥上的按钮
        } else if (mCurrentParallelRoadInfo.getHwFlag() == HW_FLAG_DOWN) {
            Logger.i(TAG, "当前在桥下，显示切换到桥上的按钮");
            mScreenView.sceneBridgeUp();
            mSwitchBridgeType = NaviConstant.LocSwitchRoadType.LOC_SWITCH_DOWN_BRIDGE_TO_UP_BRIDGE;
            updateSceneVisible(true);
            if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                mBridgeUpDownVisible.set(true);
            } else {
                Logger.i(TAG, "网络异常，不显示桥上下按钮");
            }
        }
    }

    /**
     * @param isVisible isVisible
     */
    private void updateSceneVisible(final boolean isVisible) {
        if (mScreenView == null) {
            return;
        }
        mScreenView.updateButtonVisible(mRoadMainAuxiliaryVisible.get(), mBridgeUpDownVisible.get());
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviParallelImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_PARALLEL);
    }

    /**
     * @param type 平行路切换类型 0:主辅路切换 1:桥上下切换
     */
    public void naviParallelSwitch(final int type) {
        Logger.i(TAG, "naviParallelSwitch type:", type);
        if (type == 0) {
            mSwitchActionType = mSwitchRoadType;
            requestSwitchParallelRoad();
        } else {
            mSwitchActionType = mSwitchBridgeType;
            requestSwitchParallelRoad();
        }
    }
}
