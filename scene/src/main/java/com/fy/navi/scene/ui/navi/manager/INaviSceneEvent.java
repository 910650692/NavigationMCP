package com.fy.navi.scene.ui.navi.manager;

/**
 * 卡片内部状态通知到 NaviSceneManager
 * 界面显示关闭通知、卡片状态显隐通知、其他消息通知
 */
public interface INaviSceneEvent {
    enum SceneStateChangeType {
        SceneShowState,
        SceneHideState,
        SceneCloseState
    }

    void notifySceneStateChange(SceneStateChangeType type, NaviSceneId cardId);

    void notifySceneEvent(int type, Object obj);

    /***将隐藏的scene展示***/
    void notifySceneReset();
}
