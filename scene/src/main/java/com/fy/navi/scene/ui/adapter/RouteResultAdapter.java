package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteLineInfoResultItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.utils.NumberUtils;

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
        holder.mRouteLineInfoResultItemBinding.setModel(mRouteBeanList.get(position));
        final int index = position;
        setSelectStatus(holder, mCurrentIndex == position);
        holder.mRouteLineInfoResultItemBinding.routeItemNum.setText("" + (position + NumberUtils.NUM_1));
        holder.mRouteLineInfoResultItemBinding.itemRootView.setOnClickListener(v -> {
            final boolean isSelectIndex = mCurrentIndex == index;
            if (mItemClickListener != null) {
                mItemClickListener.onItemClick(position, isSelectIndex);
            }
        });
    }
    /**
     * 设置选中道路
     * @param index 索引
     * */
    public void setSelectIndex(final int index) {
        mCurrentIndex = index;
        notifyDataSetChanged();
    }
    /**
     * 更新选中/非选中状态
     * @param holder view
     * @param select 是否选中
     * */
    private void setSelectStatus(final Holder holder, final boolean select) {
        if (select) {
            holder.mRouteLineInfoResultItemBinding.itemRootView.setBackgroundColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.bg_route_item_select));
            holder.mRouteLineInfoResultItemBinding.routeItemTag.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.bg_route_big_window_select));
            holder.mRouteLineInfoResultItemBinding.routeItemDistance.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_color_route_item_select));
            holder.mRouteLineInfoResultItemBinding.routeItemPrice.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_color_route_item_select));
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficLightValue.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_color_route_item_select));
            holder.mRouteLineInfoResultItemBinding.routeItemElectricity.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_color_route_item_select));
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficLight.setImageResource(R.drawable.img_route_money);
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficPrice.setImageResource(R.drawable.img_route_lingt);
            holder.mRouteLineInfoResultItemBinding.routeItemElectricityImg.setImageResource(R.drawable.img_route_electricity);

        } else {
            holder.mRouteLineInfoResultItemBinding.itemRootView.setBackgroundColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.bg_route_item_unselect));
            holder.mRouteLineInfoResultItemBinding.routeItemTag.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_route_defult));
            holder.mRouteLineInfoResultItemBinding.routeItemDistance.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_color_route_item_no_select));
            holder.mRouteLineInfoResultItemBinding.routeItemPrice.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_color_route_item_no_select));
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficLightValue.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_color_route_item_no_select));
            holder.mRouteLineInfoResultItemBinding.routeItemElectricity.setTextColor(
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.text_color_route_item_no_select));
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficLight.setImageResource(R.drawable.img_route_money_unselect);
            holder.mRouteLineInfoResultItemBinding.routeItemTrafficPrice.setImageResource(R.drawable.img_route_lingt_unselect);
            holder.mRouteLineInfoResultItemBinding.routeItemElectricityImg.setImageResource(R.drawable.img_route_electricity_unselect);
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