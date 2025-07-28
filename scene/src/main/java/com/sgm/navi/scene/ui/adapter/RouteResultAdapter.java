package com.sgm.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.RouteLineInfoResultItemBinding;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class RouteResultAdapter extends RecyclerView.Adapter<RouteResultAdapter.Holder> {
    private List<RouteLineInfo> mRouteBeanList;
    private OnItemClickListener mItemClickListener;
    private int mCurrentIndex = -1;

    public RouteResultAdapter() {
        mRouteBeanList = new ArrayList<>();
    }
    /**
     * 刷新列表
     * @param routeBeanList 列表数据
     * */
    public void setRouteBeanList(final List<RouteLineInfo> routeBeanList) {
        if (null == routeBeanList) {
            return;
        }

        if (routeBeanList.isEmpty()) {
            mCurrentIndex = -1;
        } else {
            mCurrentIndex = 0;
        }

        mRouteBeanList.clear();
        mRouteBeanList.addAll(routeBeanList);
        notifyDataSetChanged();
    }
    /**
     * 设置监听
     * @param itemClickListener 监听
     * */
    public void setmItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(final ViewGroup parent, final int viewType) {
        final RouteLineInfoResultItemBinding mRouteItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_line_info_result_item, parent, false);
        return new Holder(mRouteItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mRouteBeanList == null) {
            return 0;
        }
        return mRouteBeanList.size();
    }

    @Override
    public void onBindViewHolder(final Holder holder, final int position) {
        final int index = position;
        RouteLineInfo routeLineInfo = mRouteBeanList.get(position);
        setSelectStatus(holder, mCurrentIndex == position, routeLineInfo);
        if (routeLineInfo == null) {
            Logger.d("mRouteBeanList is null");
            return;
        }

        holder.mRouteLineInfoResultItemBinding.routeItemNum.setText(String.valueOf(position + NumberUtils.NUM_1));
        holder.mRouteLineInfoResultItemBinding.itemRootViewBg.setOnClickListener(v -> {
            final boolean isSelectIndex = mCurrentIndex == index;
            if (mItemClickListener != null) {
                mItemClickListener.onItemClick(position, isSelectIndex);
            }
        });

        holder.mRouteLineInfoResultItemBinding.routeItemTime.setText(routeLineInfo.getMTravelTime());
        holder.mRouteLineInfoResultItemBinding.routeItemTag.setText(routeLineInfo.getMLabel());
        if (position != 0 && routeLineInfo.getMReverseLabel() != null && !routeLineInfo.getMReverseLabel().isEmpty()) {
            holder.mRouteLineInfoResultItemBinding.routeItemReverseTag.setVisibility(View.VISIBLE);
            holder.mRouteLineInfoResultItemBinding.routeItemReverseTag.setText(routeLineInfo.getMReverseLabel());
        }else {
            holder.mRouteLineInfoResultItemBinding.routeItemReverseTag.setVisibility(View.GONE);
        }
        holder.mRouteLineInfoResultItemBinding.routeItemDistance.setText(routeLineInfo.getMLength());
        holder.mRouteLineInfoResultItemBinding.routeItemTrafficLightValue.setText(routeLineInfo.getMTollCost());
        holder.mRouteLineInfoResultItemBinding.routeItemPrice.setText(routeLineInfo.getMTrafficLightCount());

        if (routeLineInfo.isMElecRouteBool()) {
            holder.mRouteLineInfoResultItemBinding.routeItemElectricityImg.setVisibility(View.VISIBLE);
            holder.mRouteLineInfoResultItemBinding.routeItemElectricity.setVisibility(View.VISIBLE);
            holder.mRouteLineInfoResultItemBinding.routeItemElectricity.setText(routeLineInfo.getMElecRouteLabel());
            if (routeLineInfo.getMRemainPercent() < 20) {
                holder.mRouteLineInfoResultItemBinding.routeItemElectricity.setTextColor(
                        AppCache.getInstance().getMContext().getResources().getColor(R.color.text_route_restriction_text_error));
                holder.mRouteLineInfoResultItemBinding.routeItemElectricityImg.setImageResource(R.drawable.img_electricity_empty_42);
            }
        } else {
            holder.mRouteLineInfoResultItemBinding.routeItemElectricityImg.setVisibility(View.GONE);
            holder.mRouteLineInfoResultItemBinding.routeItemElectricity.setVisibility(View.GONE);
        }
    }

    @Override
    public void onBindViewHolder(final Holder holder, final int position, List<Object> payloads) {
        if (payloads.isEmpty()) {
            onBindViewHolder(holder, position);
        } else {
            setSelectStatus(holder, mCurrentIndex == position, mRouteBeanList.get(position));
        }
    }

    /**
     * 设置选中道路
     * @param index 索引
     * */
    public void setSelectIndex(final int index) {
        ThreadManager.getInstance().postUi(() -> {
            if (mCurrentIndex == index) {
                return;
            }
            int oldIndex = mCurrentIndex;
            mCurrentIndex = index;

            if (oldIndex >= 0 && oldIndex < mRouteBeanList.size()) {
                notifyItemChanged(oldIndex, new Object());
            }
            if (index >= 0 && index < mRouteBeanList.size()) {
                notifyItemChanged(index, new Object());
            }
        });
    }

    /**
     * 更新选中/非选中状态
     * @param holder view
     * @param select 是否选中
     * */
    private void setSelectStatus(final Holder holder, final boolean select, final RouteLineInfo routeLineInfo) {
        int contextColor;
        if (select) {
            contextColor = R.color.bg_route_item_select;
            holder.mRouteLineInfoResultItemBinding.routeItemTag.setTextColor(
                    AppCache.getInstance().getMContext().getResources().getColor(R.color.bg_route_big_window_select));
            holder.mRouteLineInfoResultItemBinding.routeItemReverseTag.setTextColor(
                    AppCache.getInstance().getMContext().getResources().getColor(R.color.bg_route_big_window_select));
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficLight.setImageResource(R.drawable.img_route_money);
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficPrice.setImageResource(R.drawable.img_route_lingt);
            holder.mRouteLineInfoResultItemBinding.routeItemElectricityImg.setImageResource(R.drawable.img_route_electricity);
        } else {
            contextColor = R.color.bg_route_item_unselect;
            holder.mRouteLineInfoResultItemBinding.routeItemTag.setTextColor(
                    AppCache.getInstance().getMContext().getResources().getColor(R.color.text_route_defult));
            holder.mRouteLineInfoResultItemBinding.routeItemReverseTag.setTextColor(
                    AppCache.getInstance().getMContext().getResources().getColor(R.color.text_route_defult));
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficLight.setImageResource(R.drawable.img_route_money_unselect);
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficPrice.setImageResource(R.drawable.img_route_lingt_unselect);
            holder.mRouteLineInfoResultItemBinding.routeItemElectricityImg.setImageResource(R.drawable.img_route_electricity_unselect);
        }

        holder.mRouteLineInfoResultItemBinding.itemRootView.setBackgroundColor(
                AppCache.getInstance().getMContext().getResources().getColor(contextColor));
        holder.mRouteLineInfoResultItemBinding.routeItemPrice.setTextColor(
                AppCache.getInstance().getMContext().getResources().getColor(select ? R.color.text_color_route_item_select : R.color.text_color_route_item_no_select));
        holder.mRouteLineInfoResultItemBinding.routeItemTrafficLightValue.setTextColor(
                AppCache.getInstance().getMContext().getResources().getColor(select ? R.color.text_color_route_item_select : R.color.text_color_route_item_no_select));
        holder.mRouteLineInfoResultItemBinding.routeItemElectricity.setTextColor(
                AppCache.getInstance().getMContext().getResources().getColor(select ? R.color.text_color_route_item_select : R.color.text_color_route_item_no_select));
        if (routeLineInfo.getMRemainPercent() < 20) {
            holder.mRouteLineInfoResultItemBinding.routeItemElectricity.setTextColor(
                    AppCache.getInstance().getMContext().getResources().getColor(R.color.text_route_restriction_text_error));
            holder.mRouteLineInfoResultItemBinding.routeItemElectricityImg.setImageResource(R.drawable.img_electricity_empty_42);
        }
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteLineInfoResultItemBinding mRouteLineInfoResultItemBinding;

        public Holder(final RouteLineInfoResultItemBinding routeLineInfoResultItemBinding) {
            super(routeLineInfoResultItemBinding.getRoot());
            this.mRouteLineInfoResultItemBinding = routeLineInfoResultItemBinding;
            routeLineInfoResultItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /**
         * item 点击回调
         * @param index 索引
         * @param isSelectIndex 是否选中
         * */
        void onItemClick(int index, boolean isSelectIndex);
    }
}