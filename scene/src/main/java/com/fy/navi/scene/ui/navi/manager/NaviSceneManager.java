package com.fy.navi.scene.ui.navi.manager;

import android.util.ArrayMap;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.ArrayList;

/**
 * 导航卡片管理类
 * 负责创建销毁卡片、控制卡片显隐
 */
public class NaviSceneManager implements INaviSceneEvent {
    private static final String TAG = "NaviSceneManager";
    private final ArrayMap<NaviSceneId, NaviSceneBase> mAllBaseSceneList = new ArrayMap<>();
    /*隐藏待显示*/
    private ArrayList<NaviSceneBase> mHideToShowSceneList = new ArrayList<>();
    private boolean isTouchState;

    public static NaviSceneManager getInstance() {
        return NaviSceneManager.Helper.ep;
    }

    private static final class Helper {
        private static final NaviSceneManager ep = new NaviSceneManager();
    }

    private NaviSceneManager() {
    }

    /*移除待显示scene元素*/
    public void removeHideToShowScene(NaviSceneBase scene) {
        mHideToShowSceneList.remove(scene);
    }

    /*清除待显示scene集合*/
    public void clearHideToShowScene() {
        mHideToShowSceneList.clear();
    }

    @Override
    public void notifySceneEvent(int type, Object obj) {
    }

    @Override
    public void notifySceneReset() {
        if (!ConvertUtils.isEmpty(mHideToShowSceneList)) {
            for (NaviSceneBase naviSceneBase : mHideToShowSceneList) {
                Logger.i(TAG, "notifySceneReset naviSceneBase:" + naviSceneBase.getSceneId() + ", SceneState:" + naviSceneBase.mSceneState);
                if (naviSceneBase.mSceneState != NaviSceneBase.SCENE_STATE_SHOW) {
                    naviSceneBase.show();
                }
            }
        } else {
            Logger.e(TAG, "mHideToShowSceneList is null:");
        }
    }

    /**
     * @param type   卡片关闭/打开/隐藏
     * @param cardId 卡片id
     * @brief 根据底部透出的事件显示对应卡片
     */
    @Override
    public void notifySceneStateChange(SceneStateChangeType type, NaviSceneId cardId) {
        Logger.i(TAG, "notifyCardStateChange cardId:" + cardId + ", type:" + type);
        if (type == SceneStateChangeType.SceneCloseState) {//关闭Scene
            onCloseScene(cardId);
        } else if (type == SceneStateChangeType.SceneShowState) {//显示Scene
            onShowScene(cardId);
        } else if (type == SceneStateChangeType.SceneHideState) {//隐藏Scene
            onHideScene(cardId);
        }
    }

    /***显示scene**/
    private void onShowScene(NaviSceneId showSceneId) {
        NaviSceneBase newScene = getSceneById(showSceneId);
        if (newScene == null) {
            Logger.i(TAG, "newScene == null");
            return;
        }
        Logger.i(TAG, "onShowScene showSceneId -> " + showSceneId + ",getSceneState：" + newScene.getSceneState());
        // 如果当前要显示的卡片已经show状态，则直接show进行更新不用判定
        if (newScene.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW) {
            return;
        }
        //关闭scene
        ArrayList<NaviSceneBase> closeSceneList = new ArrayList<>();
        //待显示scene
        ArrayList<NaviSceneBase> showSceneList = new ArrayList<>();
        //隐藏的scene
        ArrayList<NaviSceneBase> hideSceneList = new ArrayList<>();
        for (NaviSceneId id : mAllBaseSceneList.keySet()) {
            NaviSceneBase curScene = mAllBaseSceneList.get(id);
            if (curScene == null) {
                Logger.e(TAG, "onShowScene failed. Not find card id:" + id);
                continue;
            }
//            // 非显示卡片不check
//            if (curScene.getSceneState() != NaviSceneBase.SCENE_STATE_SHOW) {
//                continue;
//            }
            int status = NaviSceneRule.getCollisionType(id, showSceneId);
            Logger.i(TAG, "onShowScene curScene -> " + id.name() + " showSceneName -> " + showSceneId.name() + " status -> " + status);
            if (status == NaviSceneRule.SceneShowAndShow) {//新卡旧卡同时显示
                //显示新卡
                if (newScene.getSceneState() != NaviSceneBase.SCENE_STATE_SHOW && !showSceneList.contains(newScene)) {
                    showSceneList.add(newScene);
                }
                //显示旧卡(若旧卡是显示状态)
                if (curScene.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW && !showSceneList.contains(curScene)) {
                    showSceneList.add(curScene);
                }
            } else if (status == NaviSceneRule.SceneShowAndClose) {//显示旧卡关闭新卡
                //关闭新卡（若旧卡是显示状态才关闭新卡）
                if (curScene.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW &&
                        newScene.getSceneState() != NaviSceneBase.SCENE_STATE_CLOSE &&
                        !closeSceneList.contains(newScene)) {
                    closeSceneList.add(newScene);
                }
                //显示旧卡
                if (curScene.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW && !showSceneList.contains(curScene)) {
                    showSceneList.add(curScene);
                }
            } else if (status == NaviSceneRule.SceneShowAndHide) {//显示旧卡隐藏新卡
                //隐藏新卡（若旧卡是显示状态才关闭新卡）
                if (curScene.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW &&
                        newScene.getSceneState() != NaviSceneBase.SCENE_STATE_HIDE &&
                        !hideSceneList.contains(newScene)) {
                    hideSceneList.add(newScene);
                } else {
                    if (!showSceneList.contains(newScene)) {
                        showSceneList.add(curScene);
                    }
                }
                //显示旧卡
                if (curScene.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW && !showSceneList.contains(curScene)) {
                    showSceneList.add(curScene);
                }
            } else if (status == NaviSceneRule.SceneHideAndShow) {//隐藏旧卡显示新卡
                //显示新卡
                if (newScene.getSceneState() != NaviSceneBase.SCENE_STATE_SHOW && !showSceneList.contains(newScene)) {
                    showSceneList.add(newScene);
                }
                //隐藏旧卡(若旧卡显示才会隐藏)
                if (curScene.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW && !hideSceneList.contains(curScene)) {
                    hideSceneList.add(curScene);
                }
            } else if (status == NaviSceneRule.SceneCloseAndShow) {//关闭旧卡显示新卡
                //显示新卡
                if (newScene.getSceneState() != NaviSceneBase.SCENE_STATE_SHOW && !showSceneList.contains(newScene)) {
                    showSceneList.add(newScene);
                }
                //关闭旧卡
                if (curScene.getSceneState() != NaviSceneBase.SCENE_STATE_HIDE && !closeSceneList.contains(curScene)) {
                    closeSceneList.add(curScene);
                }
            } else if (status == NaviSceneRule.SceneIgnore) {    //显示新卡时，旧卡不做处理，根据自身状态显示/隐藏
                //显示新卡
                if (newScene.getSceneState() != NaviSceneBase.SCENE_STATE_SHOW && !showSceneList.contains(newScene)) {
                    showSceneList.add(newScene);
                }
            } else if (status == NaviSceneRule.SceneInvalid) {   //可以忽略
            }
        }
        Logger.i(TAG, "showSceneList:" + showSceneList.size() + ", closeSceneList:" + closeSceneList.size() + ", hideSceneList:" + hideSceneList.size());
        if (!ConvertUtils.isEmpty(showSceneList)) {
            for (NaviSceneBase naviSceneBase : showSceneList) {
                Logger.e(TAG, "show:" + naviSceneBase.mSceneId);
                naviSceneBase.show();
            }
        }
        if (!ConvertUtils.isEmpty(closeSceneList)) {
            for (NaviSceneBase naviSceneBase : closeSceneList) {
                Logger.i(TAG, "close:" + naviSceneBase.mSceneId);
                naviSceneBase.close();
            }
        }
        if (!ConvertUtils.isEmpty(hideSceneList)) {
            for (NaviSceneBase naviSceneBase : hideSceneList) {
                Logger.e(TAG, "hide:" + naviSceneBase.mSceneId);
                naviSceneBase.hide();
            }
            clearHideToShowScene();
            mHideToShowSceneList.addAll(hideSceneList);
        }
    }

    /***关闭scene**/
    private void onCloseScene(NaviSceneId closeSceneId) {
        NaviSceneBase closeScene = mAllBaseSceneList.get(closeSceneId);
        Logger.i(TAG, "onCloseScene closeSceneID -> " + closeSceneId);
        if (closeScene == null) {
            Logger.e(TAG, " onCloseScene == null -> ");
            return;
        }
        // 已经是关闭状态
        if (closeScene.getSceneState() == NaviSceneBase.SCENE_STATE_CLOSE) {
            Logger.e(TAG, " onCloseScene already close");
            return;
        }
        Logger.e(TAG, " onCloseScene closing ...");
        closeScene.close();
    }

    /***隐藏scene**/
    private void onHideScene(NaviSceneId hideSceneId) {
        NaviSceneBase hideScene = mAllBaseSceneList.get(hideSceneId);
        if (hideScene == null) {
            Logger.e(TAG, " hideScene == null -> ");
            return;
        }
        // 已经是关闭状态
        if (hideScene.getSceneState() == NaviSceneBase.SCENE_STATE_HIDE) {
            return;
        }
        Logger.i(TAG, " onHideScene hideSceneId -> " + hideSceneId);
        hideScene.hide();
        removeHideToShowScene(hideScene);
    }

    /***触摸态***/
    public void switchToTouchState() {
        isTouchState = true;
    }

    /***沉浸态***/
    public void switchToImmerState() {
        isTouchState = false;
    }

    private NaviSceneBase getSceneById(NaviSceneId id) {
        if (mAllBaseSceneList.containsKey(id)) {
            return mAllBaseSceneList.get(id);
        }
        return null;
    }

    public void addNaviScene(NaviSceneId id, NaviSceneBase naviScene) {
        Logger.i("lvww", "addNaviScene -> ", "id -> " + id , "is exit:" + (mAllBaseSceneList.get(id) != naviScene));
        if (mAllBaseSceneList.get(id) != naviScene) {
            mAllBaseSceneList.put(id, naviScene);
        } else {
            Logger.i("addNaviScene", "已存在！：" + naviScene.mSceneId.name());
        }
    }

    public void removeAllBaseScene() {
        clearHideToShowScene();
        for (NaviSceneId id : NaviSceneId.values()) {
            removeSceneById(id);
        }
    }

    private void removeSceneById(NaviSceneId id) {
        if (mAllBaseSceneList.containsKey(id)) {
            NaviSceneBase scene = mAllBaseSceneList.get(id);
            if (scene != null) {
                scene.close();
                scene.unInit();
                mAllBaseSceneList.remove(id);
            }
        } else {
            Logger.i(TAG, " removeCardById not find card id:" + id);
        }
    }

    public boolean isShow(NaviSceneId id) {
        NaviSceneBase naviScene = getSceneById(id);
        if (naviScene == null) {
            return false;
        }
        return naviScene.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW;
    }
}
