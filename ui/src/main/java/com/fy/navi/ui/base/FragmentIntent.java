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
        addFragment(screenId, containerId, fragmentManager, toFragment, bundle, true);
    }

    /**
     * 添加Fragment
     *
     * @param screenId          屏幕ID
     * @param containerId       容器ID
     * @param fragmentManager   fragmentManager
     * @param toFragment        BaseFragment
     * @param bundle            参数
     * @param isHideCurFragment 是否隐藏当前fragment
     */
    public static void addFragment(final String screenId, final int containerId,
                                   final FragmentManager fragmentManager,
                                   final BaseFragment toFragment,
                                   final Bundle bundle,
                                   final boolean isHideCurFragment) {
        if (ConvertUtils.isEmpty(toFragment)) {
            return;
        }
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        if (STACKMANAGER.isFragmentStackNull(screenId)) {
            if(Logger.openLog) {
                Logger.i(TAG, "current fragment stack is null");
            }
            toFragment.setArguments(bundle);
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            STACKMANAGER.push(screenId, toFragment);
            transaction.commitAllowingStateLoss();
            return;
        }
        final BaseFragment currentFragment = STACKMANAGER.getCurrentFragment(screenId);
        if (ConvertUtils.equals(currentFragment, toFragment)) {
            if(Logger.openLog) {
                Logger.i(TAG, "current fragment == toFragment");
            }
            toFragment.onNewIntent(bundle);
            return;
        }
        if (STACKMANAGER.isContain(screenId, toFragment)) {
            if(Logger.openLog) {
                Logger.i(TAG, "fragment stack 包含");
            }
            STACKMANAGER.removeBaseView(screenId, toFragment);
            STACKMANAGER.push(screenId, toFragment);
            if (isHideCurFragment) {
                if(currentFragment.isAdded() && currentFragment.getParentFragmentManager() == fragmentManager){
                    transaction.hide(currentFragment);
                }
            }
            transaction.show(toFragment);
            toFragment.onNewIntent(bundle);
        } else {
            if(Logger.openLog) {
                Logger.i(TAG, "fragment stack 不包含");
            }
            toFragment.setArguments(bundle);
            if (isHideCurFragment) {
                if(currentFragment.isAdded() && currentFragment.getParentFragmentManager() == fragmentManager) {
                    transaction.hide(currentFragment);
                }
            }
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
                if(Logger.openLog) {
                    Logger.i(TAG, toFragment.getClass().getSimpleName(), " 已存在，先移除");
                }
                transaction.remove(existingFragment);
                STACKMANAGER.removeBaseView(screenId, (IBaseView) existingFragment);
            }
            if (!STACKMANAGER.isFragmentStackNull(screenId)) {
                //如果查找是否存在栈顶的其他Fragment，如果有则隐藏
                final BaseFragment currentFragment = STACKMANAGER.getCurrentFragment(screenId);
                if(currentFragment.isAdded() && currentFragment.getParentFragmentManager() == fragmentManager) {
                    transaction.hide(currentFragment);
                }
            }

            if(Logger.openLog) {
                Logger.i(TAG, "添加新的 ", toFragment.getClass().getSimpleName());
            }
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
     * @param nextShow        参数
     */
    public static BaseFragment closeFragment(final String screenId, final FragmentManager fragmentManager, final boolean nextShow) {
        BaseFragment currentFragment = null;
        //syncFragmentList(screenId, fragmentManager);
        try {
            currentFragment = STACKMANAGER.popFragment(screenId);
            final FragmentTransaction transaction = fragmentManager.beginTransaction();
            if (!ConvertUtils.isEmpty(currentFragment)) {
                transaction.remove(currentFragment);
            } else {
                if (Logger.openLog) {
                    Logger.i(TAG, "对象为空无法移除", currentFragment);
                }
            }
            if (nextShow) {
                final BaseFragment toFragment = STACKMANAGER.getCurrentFragment(screenId);
                if (Logger.openLog) {
                    Logger.i(TAG, "移除上一个。显示下一个", toFragment);
                }
                if (!ConvertUtils.isEmpty(toFragment)) {
                    if (toFragment.getClass().getName().contains("NaviGuidanceFragment")) {
                        Bundle bundle = new Bundle();
                        bundle.putInt("NAVI_CONTROL", 1);
                        toFragment.onNewIntent(bundle);
                    }
                    currentFragment = toFragment;
                    if (currentFragment.getParentFragmentManager() == fragmentManager) {
                        transaction.show(toFragment);
                    }
                }
            }
            transaction.commitAllowingStateLoss();
        } catch (Exception e) {
            Logger.e(TAG, "Error in closeFragment: ", e.getMessage(), e);
        }
        return currentFragment;
    }

    public static BaseFragment closeFragment(final String screenId,
                                             final FragmentManager fragmentManager,
                                             final Bundle bundle) {
        BaseFragment currentFragment;
        currentFragment = STACKMANAGER.popFragment(screenId);
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        if (!ConvertUtils.isEmpty(currentFragment)) {
            transaction.remove(currentFragment);
        } else {
            if(Logger.openLog) {
                Logger.i(TAG, "对象为空无法移除", currentFragment);
            }
        }
        final BaseFragment toFragment = STACKMANAGER.getCurrentFragment(screenId);
        if(Logger.openLog) {
            Logger.i(TAG, "移除上一个。显示下一个", toFragment);
        }
        if (!ConvertUtils.isEmpty(toFragment)) {
            currentFragment = toFragment;
            toFragment.onNewIntent(bundle);
            if(currentFragment.getParentFragmentManager() == fragmentManager){
                transaction.show(toFragment);
            }
        }
        transaction.commitAllowingStateLoss();
        return currentFragment;
    }

    /**
     * 显示当前被hide的fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager fragmentManager
     */
    public static void showCurrentFragment(final String screenId, final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final BaseFragment toFragment = STACKMANAGER.getCurrentFragment(screenId);
        if(Logger.openLog) {
            Logger.i(TAG, "showCurrentFragment", toFragment);
        }
        if (!ConvertUtils.isEmpty(toFragment)) {
            if(Logger.openLog) {
                Logger.i(TAG, "showCurrentFragment", toFragment.isAdded(), " , ", toFragment.isHidden());
            }
            transaction.show(toFragment);
        }
        final List<Fragment> fragments = fragmentManager.getFragments();
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if(Logger.openLog) {
                    Logger.i(TAG, "showCurrentFragment", fragments.get(t));
                    Logger.i(TAG, "showCurrentFragment", fragments.get(t).isHidden());
                }
            }
        }
        transaction.commit();
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
                    if(Logger.openLog) {
                        Logger.i(TAG, "remove top index: ", t);
                    }
                }
                if (index < t && index != -1) {
                    if(Logger.openLog) {
                        Logger.i(TAG, "transaction remove top fragment: ", fragments.get(t).getClass().getName());
                    }
                    transaction.remove(fragments.get(t));
                    if (!fragments.get(t).getClass().getName().contains("SupportRequestManagerFragment")) {
                        if(Logger.openLog) {
                            Logger.i(TAG, "mStackManager remove top fragment: ", fragments.get(t).getClass().getName());
                        }
                        STACKMANAGER.removeBaseView(screenId, (IBaseView) fragments.get(t));
                    }
                }
            }
        }
        if (index != -1) {
            Fragment fragment = fragments.get(index);
            if (fragment.getClass().getName().contains("NaviGuidanceFragment")) {
                Bundle bundle = new Bundle();
                bundle.putInt("NAVI_CONTROL", 1);
                if (fragment instanceof BaseFragment<?,?>) {
                    BaseFragment baseFragment = (BaseFragment) fragment;
                    baseFragment.onNewIntent(bundle);
                }
            }
            transaction.show(fragment);
        }
        transaction.commitAllowingStateLoss();
    }

    /**
     * 关闭导航Fragment之上的所有Fragment
     * @param screenId 屏幕ID
     * @param fragmentManager FragmentManager
     */
    public static Fragment closeAllFragmentUpNavi(final String screenId,
                                              final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains("NaviGuidanceFragment")) {
                    index = t;
                    if(Logger.openLog) {
                        Logger.i(TAG, "remove top index: ", t);
                    }
                }
                if (index < t && index != -1) {
                    if(Logger.openLog) {
                        Logger.i(TAG, "transaction remove top fragment: ",
                                fragments.get(t).getClass().getName());
                    }
                    transaction.remove(fragments.get(t));
                    if (!fragments.get(t).getClass().getName().
                            contains("SupportRequestManagerFragment")) {
                        if(Logger.openLog) {
                            Logger.i(TAG, "mStackManager remove top fragment: ",
                                    fragments.get(t).getClass().getName());
                        }
                        STACKMANAGER.removeBaseView(screenId, (IBaseView) fragments.get(t));
                    }
                }
            }
        }
        Fragment fragment = null;
        if (index != -1) {
            fragment = fragments.get(index);
            transaction.show(fragment);
        }
        transaction.commitAllowingStateLoss();
        return fragment;
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
        if(Logger.openLog) {
            Logger.i(TAG, "closeAllFragmentsUntilTargetFragment className: ", className);
        }
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains(className)) {
                    index = t;
                    if(Logger.openLog) {
                        Logger.i(TAG, "remove top index: ", t);
                    }
                }
                if (index <= t && index != -1) {
                    if(Logger.openLog) {
                        Logger.i(TAG, "remove top fragment: ", fragments.get(t).getClass().getName());
                    }
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
     * @param screenId        屏幕ID
     * @param fragmentManager FragmentManager
     */
    public static void closeAllFragment(final String screenId, final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        for (Fragment fragment : fragmentManager.getFragments()) {
            transaction.remove(fragment);
        }
        transaction.commitAllowingStateLoss();
        // TODO: 2025/4/6 @xuqun 检查下我这么修改影响你的逻辑不 这么修改是为了换肤后栈集合清理不彻底的问题
        STACKMANAGER.removeAllFragment(screenId);
    }

    /**
     * 关闭所有Fragment
     */
    public void closeAllFragment() {
        STACKMANAGER.removeAllFragment();
    }

    /**
     * 同步默认栈集合和原生栈集合，应对换肤场景
     *
     * @param screenId        屏幕Id
     * @param fragmentManager 碎片管理器
     */
    public static void syncFragmentList(final String screenId, final FragmentManager fragmentManager) {
        List<Fragment> fragments = fragmentManager.getFragments();
        STACKMANAGER.removeAllFragment(screenId);
        if (!ConvertUtils.isEmpty(fragments)) {
            for (Fragment fragment : fragments) {
                if (fragment instanceof IBaseView) {
                    IBaseView baseFragment = (BaseFragment) fragment;
                    STACKMANAGER.push(screenId, baseFragment);
                }
            }
        }
        if(Logger.openLog) {
            Logger.d(TAG, "同步后的默认栈：", STACKMANAGER.getBaseFragmentStack(screenId));
        }
    }
}
