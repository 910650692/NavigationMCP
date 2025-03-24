package com.fy.navi.ui.base;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.util.List;


@SuppressLint("CommitTransaction")
public class FragmentIntent {
    private static final String TAG = FragmentIntent.class.getSimpleName();
    private static final StackManager STACKMANAGER = StackManager.getInstance();

    /**
     * 添加Fragment
     *
     * @param screenId        屏幕ID
     * @param containerId     容器ID
     * @param fragmentManager fragmentManager
     * @param toFragment      BaseFragment
     * @param bundle          参数
     */
    public static void addFragment(final String screenId, final int containerId,
                                   final FragmentManager fragmentManager,
                                   final BaseFragment toFragment,
                                   final Bundle bundle) {
        if (ConvertUtils.isEmpty(toFragment)) {
            return;
        }
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        if (STACKMANAGER.isFragmentStackNull(screenId)) {
            Logger.i(TAG, "current fragment stack is null");
            toFragment.setArguments(bundle);
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            STACKMANAGER.push(screenId, toFragment);
            transaction.commitAllowingStateLoss();
            return;
        }
        final BaseFragment currentFragment = STACKMANAGER.getCurrentFragment(screenId);
        if (ConvertUtils.equals(currentFragment, toFragment)) {
            Logger.i(TAG, "current fragment == toFragment");
            toFragment.onNewIntent(bundle);
            return;
        }
        if (STACKMANAGER.isContain(screenId, toFragment)) {
            Logger.i(TAG, "fragment stack 包含");
            STACKMANAGER.removeBaseView(screenId, toFragment);
            STACKMANAGER.push(screenId, toFragment);
            transaction.hide(currentFragment);
            transaction.show(toFragment);
            toFragment.onNewIntent(bundle);
        } else {
            Logger.i(TAG, "fragment stack 不包含");
            toFragment.setArguments(bundle);
            transaction.hide(currentFragment);
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            STACKMANAGER.push(screenId, toFragment);
        }
        transaction.commitAllowingStateLoss();
    }

    /**
     * 添加 PoiDetailsFragment
     *
     * @param screenId        屏幕ID
     * @param containerId     容器ID
     * @param fragmentManager fragmentManager
     * @param toFragment      BaseFragment
     * @param bundle          参数
     */
    public static void addPoiDetailsFragment(final String screenId, final int containerId,
                                             final FragmentManager fragmentManager,
                                             final BaseFragment toFragment,
                                             final Bundle bundle) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        try {
            // 查找是否已经存在相同类型的 PoiDetailsFragment
            Fragment existingFragment = null;
            for (Fragment fragment : fragmentManager.getFragments()) {
                if (fragment.getClass().getName().equals(toFragment.getClass().getName())) {
                    existingFragment = fragment;
                    break;
                }
            }
            // 如果已存在相同类型的 Fragment，则移除
            if (existingFragment != null) {
                Logger.i(TAG, toFragment.getClass().getSimpleName() + " 已存在，先移除");
                transaction.remove(existingFragment);
                STACKMANAGER.removeBaseView(screenId, (IBaseView) existingFragment);
            }
            if (!STACKMANAGER.isFragmentStackNull(screenId)) {
                //如果查找是否存在栈顶的其他Fragment，如果有则隐藏
                final BaseFragment currentFragment = STACKMANAGER.getCurrentFragment(screenId);
                transaction.hide(currentFragment);
            }

            Logger.i(TAG, "添加新的 " + toFragment.getClass().getSimpleName());
            toFragment.setArguments(bundle);
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            STACKMANAGER.push(screenId, toFragment);
        } catch (Exception e) {
            Logger.e(TAG, "Error in addPoiDetailsFragment: " + e.getMessage(), e);
        } finally {
            transaction.commitAllowingStateLoss();
        }
    }


    /**
     * 添加Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager fragmentManager
     * @param fragment        fragment类
     * @param nextShow        参数
     */
    public static void closeFragment(final String screenId, final FragmentManager fragmentManager,
                                     final BaseFragment fragment, final boolean nextShow) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        transaction.remove(fragment);
        STACKMANAGER.removeBaseView(screenId, fragment);
        if (nextShow) {
            Logger.i(TAG, "移除上一个。显示下一个");
            final BaseFragment toFragment = STACKMANAGER.getCurrentFragment(screenId);
            if (!ConvertUtils.isEmpty(toFragment)) {
                transaction.show(toFragment);
            }
        }
        transaction.commitAllowingStateLoss();
    }

    /**
     * 关闭所有Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager fragmentManager
     */
    public static void closeAllFragmentUpRoute(final String screenId, final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains("RouteFragment")
                        || fragments.get(t).getClass().getName().contains("NaviGuidanceFragment")) {
                    index = t;
                    Logger.i(TAG, "remove top index: " + t);
                }
                if (index < t && index != -1) {
                    Logger.i(TAG, "transaction remove top fragment: " + fragments.get(t).getClass().getName());
                    transaction.remove(fragments.get(t));
                    if (!fragments.get(t).getClass().getName().contains("SupportRequestManagerFragment")) {
                        Logger.i(TAG, "mStackManager remove top fragment: " + fragments.get(t).getClass().getName());
                        STACKMANAGER.removeBaseView(screenId, (IBaseView) fragments.get(t));
                    }
                }
            }
        }
        if (index != -1) {
            transaction.show(fragments.get(index));
        }
        transaction.commitAllowingStateLoss();
    }


    /**
     * 关闭所有Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager fragmentManager
     * @param className       目标fragment的类名
     */
    public static void closeAllFragmentsUntilTargetFragment(final String screenId,
                                                            final FragmentManager fragmentManager,
                                                            final String className) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains(className)) {
                    index = t;
                    Logger.i(TAG, "remove top index: " + t);
                }
                if (index <= t && index != -1) {
                    Logger.i(TAG, "remove top fragment: " + fragments.get(t).getClass().getName());
                    transaction.remove(fragments.get(t));
                    STACKMANAGER.removeBaseView(screenId, (IBaseView) fragments.get(t));
                }
            }
        }
        transaction.commitAllowingStateLoss();
    }

    /**
     * 关闭所有Fragment
     *
     * @param nextShow 屏幕ID
     */
    public void closeFragment(final boolean nextShow) {

    }

    /**
     * 关闭所有Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager FragmentManager
     */
    public static void closeAllFragment(final String screenId, final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        for (Fragment fragment : fragmentManager.getFragments()) {
            transaction.remove(fragment);
            if (fragment instanceof IBaseView) {
                STACKMANAGER.removeBaseView(screenId, (IBaseView) fragment);
            } else {
                Logger.e(TAG, "cannot be cast IBaseView");
            }
        }
        transaction.commitAllowingStateLoss();
    }

    /**
     * 关闭所有Fragment
     */
    public void closeAllFragment() {
        STACKMANAGER.removeAllFragment();
    }
}
