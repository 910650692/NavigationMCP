package com.fy.navi.scene.ui.navi.hangingcard;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintSet;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.R;
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
    //是否展开状态
    private boolean mIsExpand = false;
    //折叠时是否隐藏第三个
    private boolean mIsHideThird = false;
    //折叠时是否只展示一条
    private boolean mIsOnlyOne = false;

    //是否存在途径点

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
    public NaviSceneId getSceneId() {
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
    public void onCreate() {
        super.onCreate();
        mIsHideThird = getResources().getDimensionPixelSize(R.dimen.hanging_card_hide_height) == 0;
        mViewBinding.ivToggle.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                mIsExpand = !mIsExpand;
                assembleExpandLayout(mIsExpand);
            }
        });
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
        if (ConvertUtils.isNull(mViewBinding)) return;
        if (mViewBinding.llFirst.getChildCount() > 0) {
            CardView cardView = (CardView) mViewBinding.llFirst.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.pauseTimer();
            }
        }
        if (mViewBinding.llSecond.getChildCount() > 0) {
            CardView cardView = (CardView) mViewBinding.llSecond.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.pauseTimer();
            }
        }
        if (mViewBinding.llThird.getChildCount() > 0) {
            CardView cardView = (CardView) mViewBinding.llThird.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.pauseTimer();
            }
        }
    }

    private void resumeTimer() {
        if (ConvertUtils.isNull(mViewBinding)) return;
        if (mViewBinding.llFirst.getChildCount() > 0) {
            CardView cardView = (CardView) mViewBinding.llFirst.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.resumeTimer();
            }
        }
        if (mViewBinding.llSecond.getChildCount() > 0) {
            CardView cardView = (CardView) mViewBinding.llSecond.getChildAt(0);
            if (!ConvertUtils.isNull(cardView)) {
                cardView.resumeTimer();
            }
        }
        if (mViewBinding.llThird.getChildCount() > 0) {
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
        final boolean hasViaTabShow = CardManager.getInstance().getSceneOnShow(NaviSceneId.NAVI_SCENE_VIA_DETAIL_INFO);
        final boolean hasControlShow = CardManager.getInstance().getSceneOnShow(NaviSceneId.NAVI_SCENE_CONTROL);
        mIsOnlyOne = hasViaTabShow && hasControlShow && mIsHideThird;
        Logger.i(TAG, "assembleLayout hasViaTabShow:" + hasViaTabShow + " hasControlShow:" + hasControlShow + " mIsOnlyOne:" + mIsOnlyOne);
        assembleExpandLayout(mIsExpand);
    }

    /***
     * 组装折叠布局
     */
    private void assembleExpandLayout(boolean isExpand) {
        if (ConvertUtils.isNull(mScreenViewModel)) {
            return;
        }
        CopyOnWriteArrayList<HandCardType> uiList = mScreenViewModel.getUiList();
        if (ConvertUtils.isEmpty(uiList)) {
            mViewBinding.llFirst.removeAllViews();
            mViewBinding.llSecond.removeAllViews();
            mViewBinding.llThird.removeAllViews();
            mViewBinding.ivToggle.setVisibility(GONE);
            notifySceneStateChange(false);
        } else if (uiList.size() == 1) {
            addChild(uiList.get(0), mViewBinding.llFirst, true);
            mViewBinding.llFirst.setVisibility(VISIBLE);
            mViewBinding.llSecond.removeAllViews();
            mViewBinding.llThird.removeAllViews();
            mViewBinding.llSecond.setVisibility(GONE);
            mViewBinding.llThird.setVisibility(GONE);
            mViewBinding.ivToggle.setVisibility(GONE);
        } else if (uiList.size() == 2 || mIsHideThird) {
            addChild(uiList.get(0), mViewBinding.llFirst, true);
            addChild(uiList.get(1), mViewBinding.llSecond, isExpand);
            mViewBinding.llFirst.setVisibility(VISIBLE);
            mViewBinding.llSecond.setVisibility(VISIBLE);
            mViewBinding.llThird.removeAllViews();
            mViewBinding.llThird.setVisibility(GONE);
            mViewBinding.ivToggle.setVisibility(VISIBLE);
            int secondTop = getResources().getDimensionPixelSize(R.dimen.hanging_card_second_unexpand);
            if (isExpand) {
                secondTop = getResources().getDimensionPixelSize(R.dimen.hanging_card_second_expand);
            } else {
                if (mIsOnlyOne) {
                    secondTop = 0;
                }
            }
            ConstraintSet constraintSet = new ConstraintSet();
            constraintSet.clone(mViewBinding.clHangingCard);
            constraintSet.connect(mViewBinding.llSecond.getId(), ConstraintSet.TOP, ConstraintSet.PARENT_ID, ConstraintSet.TOP, secondTop);
            constraintSet.applyTo(mViewBinding.clHangingCard);
            Logger.i(TAG, "assembleExpandLayout secondTop:" + secondTop);
        } else if (uiList.size() == 3) {
            addChild(uiList.get(0), mViewBinding.llFirst, true);
            addChild(uiList.get(1), mViewBinding.llSecond, isExpand);
            addChild(uiList.get(2), mViewBinding.llThird, isExpand);
            mViewBinding.llFirst.setVisibility(VISIBLE);
            mViewBinding.llSecond.setVisibility(VISIBLE);
            mViewBinding.llThird.setVisibility(VISIBLE);
            mViewBinding.ivToggle.setVisibility(VISIBLE);
            int secondTop = getResources().getDimensionPixelSize(R.dimen.hanging_card_second_unexpand);
            int thirdTop = getResources().getDimensionPixelSize(R.dimen.hanging_card_third_unexpand);
            if (isExpand) {
                secondTop = getResources().getDimensionPixelSize(R.dimen.hanging_card_second_expand);
                thirdTop = getResources().getDimensionPixelSize(R.dimen.hanging_card_third_expand);
            } else {
                if (mIsOnlyOne) {
                    secondTop = 0;
                    thirdTop = 0;
                } else if (mIsHideThird) {
                    thirdTop = 0;
                }
            }
            ConstraintSet constraintSet = new ConstraintSet();
            constraintSet.clone(mViewBinding.clHangingCard);
            constraintSet.connect(mViewBinding.llSecond.getId(), ConstraintSet.TOP, ConstraintSet.PARENT_ID, ConstraintSet.TOP, secondTop);
            constraintSet.connect(mViewBinding.llThird.getId(), ConstraintSet.TOP, ConstraintSet.PARENT_ID, ConstraintSet.TOP, thirdTop);
            constraintSet.applyTo(mViewBinding.clHangingCard);
            Logger.i(TAG, "assembleExpandLayout secondTop:" + secondTop + " thirdTop:" + thirdTop);
        }
        Logger.i(TAG, "assembleExpandLayout size:" + uiList.size() + " isExpand:" + isExpand + " mIsHideThird:" + mIsHideThird + " mIsOnlyOne:" + mIsOnlyOne);
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
            addChild(uiList.get(0), mViewBinding.llFirst, false);
            mViewBinding.llSecond.removeAllViews();
            mViewBinding.llThird.removeAllViews();
        } else if (uiList.size() == 2) {
            addChild(uiList.get(1), mViewBinding.llSecond, false);
        }
        Logger.i(TAG, "assembleUnExpandLayout");
    }

    private void addChild(HandCardType type, ViewGroup parent, boolean isExpand) {
        final CardView newCardView = CardManager.getInstance().createCardViewByType(this, type);
        if (parent.getChildCount() > 0) {
            CardView cardView = (CardView) parent.getChildAt(0);
            if (!ConvertUtils.isNull(cardView) && cardView.mType != type) {
                parent.removeAllViews();
                parent.addView(newCardView);
                newCardView.setExpandState(isExpand);
                if (isVisible()) {
                    newCardView.startTimer();
                }
            } else {
                cardView.setExpandState(isExpand);
            }
        } else {
            parent.addView(newCardView);
            newCardView.setExpandState(isExpand);
            if (isVisible()) {
                newCardView.startTimer();
            }
        }
    }
}
