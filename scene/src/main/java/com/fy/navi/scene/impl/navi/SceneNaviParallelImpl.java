package com.fy.navi.scene.impl.navi;


import static com.fy.navi.service.adapter.navi.NaviConstant.LocSwitchRoadType.LocSwitchDownBridgeToUpBridge;
import static com.fy.navi.service.adapter.navi.NaviConstant.LocSwitchRoadType.LocSwitchMainToSide;
import static com.fy.navi.service.adapter.navi.NaviConstant.LocSwitchRoadType.LocSwitchNull;
import static com.fy.navi.service.adapter.navi.NaviConstant.LocSwitchRoadType.LocSwitchSideToMain;
import static com.fy.navi.service.adapter.navi.NaviConstant.LocSwitchRoadType.LocSwitchUpBridgeToDownBridge;
import static com.fy.navi.service.adapter.navi.NaviConstant.LocSwitchRoadType.AUTO_UNKNOWN_ERROR;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviParallelView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.position.LocalParallelRoadEntity;
import com.fy.navi.service.logicpaket.position.IPositionPackageCallback;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.ui.action.Action;

import java.math.BigInteger;
import java.util.ArrayList;

public class SceneNaviParallelImpl extends BaseSceneModel<SceneNaviParallelView> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final PositionPackage mPositionPackage;
    private ISceneCallback mISceneCallback;
    private LocParallelInfoEntity mCurrentParallelRoadInfo;
    private LocParallelInfoEntity mPreviousParallelRoadInfo;
    /***按钮点击动作类型***/
    private int mSwitchRoadType;
    private int mSwitchBridgeType;
    private int mSwitchActionType = LocSwitchNull;
    private boolean mIsSwitchParallelEnabled = true;
    public static final int PARALLELROAD_FLAG_MAIN = 1;
    public static final int PARALLELROAD_FLAG_SIDE = 2;
    public static final int HW_FLAG_UP = 1;
    public static final int HW_FLAG_DOWN = 2;
    // 上下桥切换按钮是否可见
    public ObservableField<Boolean> bridgeUpDownVisible;
    // 主辅路切换按钮是否可见
    public ObservableField<Boolean> roadMainAuxiliaryVisible;

    public SceneNaviParallelImpl(SceneNaviParallelView mScreenView) {
        super(mScreenView);
        mPositionPackage = PositionPackage.getInstance();
        mPositionPackage.registerCallBack(iPositionPackageCallback);
        bridgeUpDownVisible = new ObservableField<>(false);
        roadMainAuxiliaryVisible = new ObservableField<>(false);
    }

    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    public Action clickBridgeUpDown = () -> {
        Logger.i(TAG, "clickBridgeUpDown ");
        mSwitchActionType = mSwitchBridgeType;
        requestSwitchParallelRoad();
    };

    public Action clickMainAuxiliary = () -> {
        Logger.i(TAG, "clickMainAuxiliary ");
        mSwitchActionType = mSwitchRoadType;
        requestSwitchParallelRoad();
    };

    @Override
    protected void onDestroy() {
        mPositionPackage.unregisterCallBack(iPositionPackageCallback);
        super.onDestroy();
    }

    // TODO:理解直接触发算路就行了后续应该不用处理
    public void notifySwitchParallelRouteResult(boolean routeSuccess) {
        Logger.i("notifySwitchParallelRouteResult " + routeSuccess);
        // 收到平行算路结果，表示切换动作全部完成
        mIsSwitchParallelEnabled = true;
        if (mSwitchActionType == LocSwitchMainToSide) {
            mScreenView.showToastRoadMainToSide();
        } else if (mSwitchActionType == LocSwitchSideToMain) {
            mScreenView.showToastRoadSideToMain();
        } else if (mSwitchActionType == LocSwitchUpBridgeToDownBridge) {
            mScreenView.showToastBridgeUpToDown();
        } else if (mSwitchActionType == LocSwitchDownBridgeToUpBridge) {
            mScreenView.showToastBridgeDownToUp();
        }
        mSwitchActionType = LocSwitchNull;
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
            mPositionPackage.switchParallelRoad(mSwitchActionType, BigInteger.ZERO);
        } else {
            Logger.i(TAG, "网络异常，进行离线平行路切换");
            BigInteger parallelRoadId = getOffLineParallelRoadId(mSwitchActionType);
            mPositionPackage.switchParallelRoad(mSwitchActionType, parallelRoadId);
        }
        mIsSwitchParallelEnabled = false;
    }

    /**
     * 获取离线切换的平行路道路ID
     * @param switchActionType 平行路切换类型 -1：不切换 0：主路切换到辅路 1：辅路切换到主路
     * @return 离线切换的平行路道路ID
     */
    private BigInteger getOffLineParallelRoadId(int switchActionType) {
        return switch (switchActionType) {
            case LocSwitchMainToSide -> getOffLineSideRoadId();
            case LocSwitchSideToMain -> getOffLineMainRoadId();
            default -> BigInteger.ZERO;
        };
    }

    /**
     * 获取离线模式下的辅路道路ID
     * @return 离线切换的辅路道路ID
     */
    private BigInteger getOffLineSideRoadId() {
        if (mCurrentParallelRoadInfo != null) {
            ArrayList<LocalParallelRoadEntity> list = mCurrentParallelRoadInfo.
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
            ArrayList<LocalParallelRoadEntity> list = mCurrentParallelRoadInfo.
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

    public int switchParallelRoadAndBridge(int actionType) {
        if (actionType == LocSwitchMainToSide || actionType == LocSwitchSideToMain) {
            if (mSwitchRoadType == LocSwitchNull) {
                return LocSwitchNull;
            }
            if (mSwitchRoadType != actionType) {
                return AUTO_UNKNOWN_ERROR;
            }
        } else if (actionType == LocSwitchDownBridgeToUpBridge || actionType == LocSwitchUpBridgeToDownBridge) {
            if (mSwitchBridgeType == LocSwitchNull) {
                return LocSwitchNull;
            }
            if (mSwitchBridgeType != actionType) {
                return AUTO_UNKNOWN_ERROR;
            }
        }
        mSwitchActionType = mSwitchBridgeType;
        requestSwitchParallelRoad();
        return mSwitchBridgeType;
    }

    private IPositionPackageCallback iPositionPackageCallback = new IPositionPackageCallback() {
        @Override
        public void onSwitchParallelRoadFinished() {
            Logger.i(TAG, "onSwitchParallelRoadFinished ");
            // 这边开启了平行路切换后自动算路，所以不进行手动切换
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
            Logger.i(TAG, "onParallelRoadUpdate " + entity.toString());
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    if (mISceneCallback == null) {
                        return;
                    }
                    if (ConvertUtils.isEmpty(entity)) {
                        Logger.i(TAG, "entity is null");
                        hideSwitchParallelRoadUi();
                        return;
                    }
                    mCurrentParallelRoadInfo = entity;
                    refreshRoadBridgeUi();
                }
            });
        }
    };

    /**
     * 隐藏平行路切换UI
     */
    private void hideSwitchParallelRoadUi() {
        mSwitchRoadType = LocSwitchNull;
        mSwitchBridgeType = LocSwitchNull;
        roadMainAuxiliaryVisible.set(false);
        bridgeUpDownVisible.set(false);
        updateSceneVisible(false);
    }

    private void refreshRoadBridgeUi() {
        Logger.i(TAG, "refreshRoadBridgeUi ");
        if (mISceneCallback == null) {
            return;
        }
        Logger.i(TAG, "mCurrentParallelRoadInfo：" + mCurrentParallelRoadInfo.toString());
        // 平行路切换功能隐藏
        if (mCurrentParallelRoadInfo == null) {
            roadMainAuxiliaryVisible.set(false);
            bridgeUpDownVisible.set(false);
            updateSceneVisible(false);
            return;
        }

        if (mPreviousParallelRoadInfo != null) {
            Logger.i(TAG, "mPreviousParallelRoadInfo：" +
                    mPreviousParallelRoadInfo.toString());
            // 如果主辅路和桥上下没有变化，直接返回
            if (mPreviousParallelRoadInfo.getFlag() == mCurrentParallelRoadInfo.getFlag() &&
                    mPreviousParallelRoadInfo.getHwFlag() == mCurrentParallelRoadInfo.getHwFlag()) {
                return;
            }
        }
        // 保存并刷新当前主辅路、桥上下
        mPreviousParallelRoadInfo = mCurrentParallelRoadInfo;
        hideSwitchParallelRoadUi();

        // 不显示按钮的场景：切换过程中、断网场景、车道级导航中
        if (1 == mCurrentParallelRoadInfo.getStatus()) {
            Logger.i(TAG, "refreshRoadBridgeUi skip parallel due to laneState or status");
            return;
        }

        // 显示切换到辅路的按钮
        if (mCurrentParallelRoadInfo.getFlag() == PARALLELROAD_FLAG_MAIN) {
            mScreenView.sceneRoadSide();
            mSwitchRoadType = LocSwitchMainToSide;
            roadMainAuxiliaryVisible.set(true);
            updateSceneVisible(true);
            //显示切换到主路的按钮
        } else if (mCurrentParallelRoadInfo.getFlag() == PARALLELROAD_FLAG_SIDE) {
            mScreenView.sceneRoadMain();
            mSwitchRoadType = LocSwitchSideToMain;
            roadMainAuxiliaryVisible.set(true);
            updateSceneVisible(true);
        }
        // 显示切换到桥下的按钮
        if (mCurrentParallelRoadInfo.getHwFlag() == HW_FLAG_UP) {
            mScreenView.sceneBridgeDown();
            mSwitchBridgeType = LocSwitchUpBridgeToDownBridge;
            updateSceneVisible(true);
            // 如果网络正常，才显示桥上下按钮
            if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                bridgeUpDownVisible.set(true);
            }
            // 显示切换到桥上的按钮
        } else if (mCurrentParallelRoadInfo.getHwFlag() == HW_FLAG_DOWN) {
            mScreenView.sceneBridgeUp();
            mSwitchBridgeType = LocSwitchDownBridgeToUpBridge;
            updateSceneVisible(true);
            if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                bridgeUpDownVisible.set(true);
            }
        }
    }

    private void updateSceneVisible(boolean isVisible) {
        Logger.i(TAG, "updateSceneVisible isVisible:" + isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_PARALLEL);
    }
}
