package com.fy.navi.scene.ui.navi.hangingcard;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.databinding.HangingCardLayoutBinding;
import com.fy.navi.scene.impl.navi.SceneNaviHangingCardImpl;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.util.HandCardType;

import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/14
 * Description: [悬挂卡，状态比较多]
 * -----------------------------------------
 */
public class NaviSceneHangingCard extends NaviSceneBase<HangingCardLayoutBinding, SceneNaviHangingCardImpl> {
    private static final String TAG = "NaviSceneHangingCard";

    public NaviSceneHangingCard(@NonNull Context context) {
        super(context);
    }

    public NaviSceneHangingCard(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public NaviSceneHangingCard(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SUSPEND_CARD;
    }

    @Override
    protected HangingCardLayoutBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return HangingCardLayoutBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviHangingCardImpl initSceneImpl() {
        return new SceneNaviHangingCardImpl(this);
    }

    public SceneNaviHangingCardImpl getImpl() {
        return mScreenViewModel;
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {

    }

    @Override
    public void hide() {
        super.hide();
        // 隐藏的时候停止倒计时
        pauseTimer();
    }

    @Override
    public void show() {
        super.show();
        resumeTimer();
    }

    private void pauseTimer() {
        if (mViewBinding.llFirst.getChildCount()>0) {
           CardView cardView = (CardView) mViewBinding.llFirst.getChildAt(0);
           if (!ConvertUtils.isNull(cardView)) {
               cardView.pauseTimer();
           }
        }
        if (mViewBinding.llSecond.getChildCount()>0) {
            CardView cardView = (CardView) mViewBinding.llSecond.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.pauseTimer();
            }
        }
        if (mViewBinding.llThird.getChildCount()>0) {
            CardView cardView = (CardView) mViewBinding.llThird.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.pauseTimer();
            }
        }
    }

    private void resumeTimer() {
        if (mViewBinding.llFirst.getChildCount()>0) {
            CardView cardView = (CardView) mViewBinding.llFirst.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.resumeTimer();
            }
        }
        if (mViewBinding.llSecond.getChildCount()>0) {
            CardView cardView = (CardView) mViewBinding.llSecond.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.resumeTimer();
            }
        }
        if (mViewBinding.llThird.getChildCount()>0) {
            CardView cardView = (CardView) mViewBinding.llThird.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.resumeTimer();
            }
        }
    }

    /***
     * 数据发生变化更新UI
     *这里只负责更新数据
     */
    public synchronized void notifyDataChanged() {
        ThreadManager.getInstance().postUi(() -> assembleLayout());
    }

    /***
     * 重新布局UI
     * 1.空间足够，布局无需折叠显示
     *   a.只有一条数据
     *   b.有两条且途径点tab不显示
     * 2.空间不足，布局折叠显示
     *   a.有两条数据且有途径点tab且控制栏正在显示
     *
     *   注意：控制栏是长显，除非主动操作展开，但是主动操作后不要忘记再把它显示出来
     */
    private synchronized void assembleLayout() {
        if (ConvertUtils.isNull(mScreenViewModel)) return;
        final int showSize = mScreenViewModel.getUiList().size();
        final boolean hasViaTabShow = CardManager.getInstance().getSceneOnShow(NaviSceneId.NAVI_SCENE_VIA_DETAIL_INFO);
        final boolean hasControlShow = CardManager.getInstance().getSceneOnShow(NaviSceneId.NAVI_SCENE_CONTROL);
        if (showSize >= 2 && hasViaTabShow && hasControlShow) {
            assembleExpandLayout();
        } else {
            assembleUnExpandLayout();
        }
    }

    /***
     * 组装折叠布局
     */
    private void assembleExpandLayout() {
        if (ConvertUtils.isNull(mScreenViewModel)) {
            return;
        }
        CopyOnWriteArrayList<HandCardType> uiList = mScreenViewModel.getUiList();
        addChild(uiList.get(0), mViewBinding.llFirst);
        addChild(uiList.get(1), mViewBinding.llThird);
        Logger.i(TAG, "assembleExpandLayout");
    }

    /***
     * 组装展开布局
     */
    private void assembleUnExpandLayout() {
        if (ConvertUtils.isNull(mScreenViewModel)) {
            return;
        }
        CopyOnWriteArrayList<HandCardType> uiList = mScreenViewModel.getUiList();
        if (ConvertUtils.isEmpty(uiList)) {
            mViewBinding.llFirst.removeAllViews();
            mViewBinding.llSecond.removeAllViews();
            mViewBinding.llThird.removeAllViews();
            notifySceneStateChange(false);
        } else if (uiList.size() == 1) {
            addChild(uiList.get(0), mViewBinding.llFirst);
            mViewBinding.llSecond.removeAllViews();
            mViewBinding.llThird.removeAllViews();
        } else if (uiList.size() == 2) {
            addChild(uiList.get(1), mViewBinding.llSecond);
        }
        Logger.i(TAG, "assembleUnExpandLayout");
    }

    private void addChild(HandCardType type, ViewGroup parent) {
        final CardView newCardView = CardManager.getInstance().createCardViewByType(this, type);
        if (parent.getChildCount() > 0) {
            CardView cardView = (CardView) parent.getChildAt(0);
            if (!ConvertUtils.isNull(cardView) && cardView.mType != type) {
                parent.removeAllViews();
                parent.addView(newCardView);
                if (isVisible()) {
                    newCardView.startTimer();
                }
            }
        } else {
            parent.addView(newCardView);
            if (isVisible()) {
                newCardView.startTimer();
            }
        }
    }
}
