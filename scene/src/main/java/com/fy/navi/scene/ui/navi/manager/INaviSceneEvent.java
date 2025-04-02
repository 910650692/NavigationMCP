package com.fy.navi.scene.ui.navi.manager;

/**
 * 卡片内部状态通知到 NaviSceneManager
 * 界面显示关闭通知、卡片状态显隐通知、其他消息通知
 * @author fy
 * @version $Revision.*$
 */
public interface INaviSceneEvent {
    enum SceneStateChangeType {
        SceneShowState,
        SceneHideState,
        SceneCloseState
    }

    /**
     * @param type type
     * @param cardId cardId
     */
    void notifySceneStateChange(SceneStateChangeType type, NaviSceneId cardId);

    /***将隐藏的scene展示***/
    void notifySceneReset();

    void destroySceneView();

    void onCreateSceneView();
}
