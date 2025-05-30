package com.fy.navi.scene.ui.navi.manager;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;

/**
 * 定义碰撞逻辑的matrix表;
 * 以2维数组表示，行代表的是新卡片，列代表的是当前显示卡片(旧卡)；
 * 显示优先级根据UE及产品文档定义;
 * <p>
 * 卡片显隐规则如下:
 * 双卡同时显示：   SceneShowAndShow    0
 * 显示旧卡关闭新卡：SceneShowAndClose   1
 * 显示旧卡隐藏新卡：SceneShowAndHide    2
 * 隐藏旧卡显示新卡：SceneHideAndShow    3
 * 关闭旧卡显示新卡：SceneCloseAndShow   4
 * *************：SceneInvalid        5 无效场景 什么也不做
 * <p>
 * 20250324最后一次更新，后续在云文档中查看碰撞规则：https://thundersoft.feishu.cn/wiki/T5f7wpRVWiO5q7k4yXYcz8EInpb
 *
 * @author fy
 * @version $Revision.*$
 */
public final class NaviSceneRule {

    private NaviSceneRule() {

    }

    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    public final static int SCENE_INVALID = 5;
    public final static int SCENE_SHOW_AND_SHOW = 0;
    public final static int SCENE_SHOW_AND_CLOSE = 1;
    public final static int SCENE_SHOW_AND_HIDE = 2;
    public final static int SCENE_HIDE_AND_SHOW = 3;
    public final static int SCENE_CLOSE_AND_SHOW = 4;

    /**
     * 0:2D路口大图 1:3D路口大图 2:eta信息 3:车道线信息 4:tbt信息 5:途经点列表
     * 6:五公里途经点详情展示 7:途经点tab显示 8:服务区卡片 9:底部控制按钮 10:平行路切换
     * 11:底部控制按钮展开更多 12:路线偏好 13:区间测速 14:路况条信息 15:当前道路名称
     * 16:途经点到达弹窗 17:sapa详情页 18:行程报告页 19:最后一公里 20:EvCar消息
     * 21:悬挂卡 22:继续导航 23:悬挂卡-详情
     */
    private static final int[][] COLLISION_MATRIX = {//[9,23]
            /*    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23}*/ //新卡
            /*00*/{5, 1, 0, 3, 0, 1, 3, 3, 3, 0, 0, 1, 1, 0, 3, 0, 0, 5, 1, 0, 0, 3, 0, 4},
            /*01*/{1, 5, 0, 3, 0, 1, 3, 3, 3, 0, 0, 1, 1, 0, 3, 0, 0, 5, 1, 0, 0, 3, 0, 4},
            /*02*/{0, 0, 5, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 1, 0, 0, 0, 0, 0},
            /*03*/{2, 2, 0, 5, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 1, 0, 0, 2, 0, 0},
            /*04*/{0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 1, 0, 0, 0, 0, 0},
            /*05*/{4, 4, 3, 3, 0, 5, 3, 3, 3, 3, 0, 4, 4, 0, 3, 0, 0, 5, 1, 0, 3, 3, 0, 3},
            /*06*/{2, 2, 0, 0, 0, 2, 5, 0, 3, 0, 0, 2, 2, 0, 0, 0, 0, 5, 5, 0, 2, 3, 0, 5},
            /*07*/{2, 2, 0, 0, 0, 2, 0, 5, 3, 0, 0, 0, 2, 0, 0, 0, 0, 5, 5, 0, 0, 0, 0, 0},
            /*08*/{2, 2, 0, 0, 0, 2, 2, 0, 5, 0, 0, 2, 2, 0, 0, 0, 0, 5, 1, 0, 3, 3, 5, 2},
            /*09*/{0, 0, 0, 0, 0, 2, 0, 0, 0, 5, 0, 4, 4, 0, 0, 0, 0, 4, 1, 0, 0, 0, 0, 4},
            /*10*/{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0},
            /*11*/{4, 4, 0, 0, 0, 1, 3, 0, 3, 3, 4, 5, 3, 0, 0, 0, 0, 5, 1, 0, 0, 3, 0, 0},
            /*12*/{4, 4, 0, 0, 0, 1, 3, 3, 3, 3, 4, 3, 5, 0, 0, 0, 0, 5, 1, 0, 0, 3, 0, 0},
            /*13*/{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0},
            /*14*/{2, 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 5, 1, 0, 0, 0, 0, 0},
            /*15*/{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 1, 0, 0, 0, 1, 0},
            /*16*/{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 1, 4, 3, 3, 0, 0},
            /*17*/{4, 4, 3, 3, 3, 5, 3, 3, 3, 3, 4, 4, 4, 0, 3, 0, 0, 5, 1, 0, 3, 3, 0, 0},
            /*18*/{4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 4, 4, 5, 4, 3, 3, 0, 0},
            /*19*/{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 5, 3, 3, 0, 0},
            /*20*/{0, 0, 0, 0, 0, 2, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 5, 3, 0, 3},
            /*21*/{2, 2, 0, 0, 0, 2, 2, 0, 3, 0, 0, 2, 2, 0, 0, 0, 0, 3, 3, 3, 3, 5, 0, 5},
            /*22*/{0, 0, 0, 0, 5, 0, 0, 0, 0, 5, 0, 0, 0, 0, 5, 4, 0, 0, 0, 0, 0, 0, 5, 0},
            /*23*/{2, 2, 0, 0, 0, 2, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 2, 3, 0, 5}
    };

    /**
     * @param curCard 列
     * @param newCard 行
     * @return 碰撞结果
     */
    public static int getCollisionType(final NaviSceneId curCard, final NaviSceneId newCard) {
        final int curIndex = curCard.ordinal();//列
        final int newIndex = newCard.ordinal();//行
        Logger.i("NaviSceneRule", "行 curCard：" + curCard + ",curIndex：" + curIndex + "\n" + "列 newCard：" + newCard + ",newIndex：" + newIndex);
        final int maxIndex = NaviSceneId.values().length - 1;
        Logger.i("NaviSceneRule", "maxIndex：" + maxIndex + ",COLLISION_MATRIX.length：" + COLLISION_MATRIX.length);
        //非法值;
        if (curIndex > maxIndex || newIndex > maxIndex ||
                curIndex >= COLLISION_MATRIX.length || newIndex >= COLLISION_MATRIX.length) {
            Logger.e(TAG, "SceneInvalid");
            return SCENE_INVALID;
        }
        return COLLISION_MATRIX[newIndex][curIndex];//第newIndex行 第curIndex列
    }
}
