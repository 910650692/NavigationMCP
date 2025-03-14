package com.fy.navi.hmi.mapdata.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;
import java.util.List;

public class SearchMapDataAdapter extends BaseSearchMapDataAdapter<
        SearchMapDataAdapter.GroupItemViewHolder, SearchMapDataAdapter.SubItemViewHolder> {

    private static final String TAG = SearchMapDataAdapter.class.getSimpleName();
    private Context context;
    private OfflineItemListener offlineItemListener;
    private List<DataTree<ProvDataInfo, String>> dts = new ArrayList<>();

    public SearchMapDataAdapter(Context context) {
        this.context = context;
    }

    public void setData(List datas) {
        dts = datas;
        notifyNewData(dts);
    }

    public void setOfflineItemListener(OfflineItemListener listener) {
        offlineItemListener = listener;
    }

    @Override
    public BaseRecyclerHolder groupItemViewHolder(ViewGroup parent) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_province, parent, false);
        return new GroupItemViewHolder(view);
    }

    @Override
    public BaseRecyclerHolder subItemViewHolder(ViewGroup parent) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_city_data, parent, false);
        return new SubItemViewHolder(view);
    }

    @Override
    public void onGroupItemBindViewHolder(BaseRecyclerHolder holder, int groupItemIndex) {
        if (dts != null && !dts.isEmpty()) {
            String groupName = dts.get(groupItemIndex).getGroupItem().name;
            holder.setText(R.id.item_province_name, groupName); //省份名称
            int areaType = dts.get(groupItemIndex).getGroupItem().areaType;
            // item 若为城市，则显示城市下载状态；若为省份，则显示展开/关闭按钮
            if (areaType == 2 || areaType == 3) {
                holder.setVisible(R.id.item_download_status, true);
                holder.setVisible(R.id.item_province_data, true);
                holder.setVisible(R.id.item_province_expand, false);

                CityDownLoadInfo downloadItem = dts.get(groupItemIndex).getGroupItem().downLoadInfo;
                String sizeString = StringUtils.formatSize(downloadItem.nFullZipSize);
                holder.setText(R.id.item_province_data, sizeString); //城市数据包大小

            } else {
                holder.setVisible(R.id.item_download_status, false);
                holder.setVisible(R.id.item_province_data, false);
                holder.setVisible(R.id.item_province_expand, true);
            }
        }
    }

    @Override
    public void onSubItemBindViewHolder(BaseRecyclerHolder holder, int groupItemIndex, int subItemIndex) {
        if (dts != null && !dts.isEmpty()) {
            //城市名称
            String cityName = dts.get(groupItemIndex).getSubItems().get(subItemIndex).name;
            holder.setText(R.id.item_city_name, cityName);
            //城市下载状态
            CityDownLoadInfo downloadItem = dts.get(groupItemIndex).getSubItems().get(subItemIndex).downLoadInfo;
            holder.setText(R.id.item_status_tip, downloadItem.statusTip);
            //城市数据包大小
            String sizeString = StringUtils.formatSize(downloadItem.nFullZipSize);
            holder.setText(R.id.item_city_data, sizeString);
        }
    }

    @Override
    public void onGroupItemClick(Boolean isExpand, GroupItemViewHolder holder, int groupItemIndex) {
        Logger.d(TAG, "group item " + groupItemIndex + " is expand " + isExpand);

        int areaType = dts.get(groupItemIndex).getGroupItem().areaType;
        // item 若为城市，则显示城市下载状态；若为省份，则显示展开/关闭按钮
        if (areaType == 2 || areaType == 3) {
            // TODO: 2025/2/17
        } else {

            if (isExpand) {
                holder.setImageResource(R.id.item_province_expand,R.drawable.img_under_the_58);
            } else {
                holder.setImageResource(R.id.item_province_expand, R.drawable.img_up_58);
            }

        }
    }

    @Override
    public void onSubItemClick(SubItemViewHolder holder, int groupItemIndex, int subItemIndex) {
        Logger.d(TAG, "sub item " + subItemIndex + " in group item " + groupItemIndex);

      /*  CityInfo item = dts.get(groupItemIndex).getSubItems().get(subItemIndex);
        if (offlineItemListener != null) {
            if (mDownloadBtnView.getIsCanDelete()) {
                offlineItemListener.delete(item.getId(), item.getName());
                return;
            }
            switch (item.getStatus()) {
                case DownloadStatus.NOT_DOWNLOADED:
                case DownloadStatus.NEED_UPDATE:
                case DownloadStatus.SUSPEND:
                case DownloadStatus.ERROR:
                case DownloadStatus.ERROR_UPDATE:
                    long size = item.getTotalSize();
                    if (item.getStatus() == DownloadStatus.NEED_UPDATE
                            || item.getStatus() == DownloadStatus.ERROR_UPDATE) {
                        size = item.getUpdateSize();
                    } else if (item.getStatus() == DownloadStatus.SUSPEND) {
                        if (item.getUpdateSize() != 0) {
                            size = item.getUpdateSize() - item.getDownloadUpdateSize();
                        } else {
                            size = item.getTotalSize() - item.getDownloadSize();
                        }
                    }
                    offlineItemListener.download(item.getId(), size);
                    break;
                case DownloadStatus.DOWNLOADING:
                case DownloadStatus.UPDATING:
                case DownloadStatus.WAITING:
                case DownloadStatus.WAITING_UPDATE:
                    offlineItemListener.suspend(item.getId());
                    break;
                default:
                    break;
            }
        }*/

    }

    public static class GroupItemViewHolder extends BaseRecyclerHolder {
        public GroupItemViewHolder(View itemView) {
            super(itemView);
        }
    }

    public static class SubItemViewHolder extends BaseRecyclerHolder {
        public SubItemViewHolder(View itemView) {
            super(itemView);
        }
    }

    public interface OfflineItemListener {
        void startAllTask(int downloadMode);
        void pauseAllTask(int downloadMode);
        void cancelAllTask(int downloadMode);

    }

}
